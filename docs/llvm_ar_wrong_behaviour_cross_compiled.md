# Host Triple

LLVM_HOST_TRIPLE is set according to platform we are compiling from, which means the cross-compiled version of llvm ar bundled with zig returns the wrong value from getProcessTriple().

https://github.com/llvm-mirror/llvm/blob/master/tools/llvm-ar/llvm-ar.cpp
```C++
static object::Archive::Kind getDefaultForHost() {
  return Triple(sys::getProcessTriple()).isOSDarwin()
             ? object::Archive::K_DARWIN
             : object::Archive::K_GNU;
}
```

https://github.com/llvm-mirror/llvm/blob/master/lib/Support/Triple.cpp
```C++
/// Construct a triple from the string representation provided.
///
/// This stores the string representation and parses the various pieces into
/// enum members.
Triple::Triple(const Twine &Str)
    : Data(Str.str()), Arch(UnknownArch), SubArch(NoSubArch),
      Vendor(UnknownVendor), OS(UnknownOS), Environment(UnknownEnvironment),
      ObjectFormat(UnknownObjectFormat) {
  // Do minimal parsing by hand here.
  SmallVector<StringRef, 4> Components;
  StringRef(Data).split(Components, '-', /*MaxSplit*/ 3);
  if (Components.size() > 0) {
    Arch = parseArch(Components[0]);
    SubArch = parseSubArch(Components[0]);
    if (Components.size() > 1) {
      Vendor = parseVendor(Components[1]);
      if (Components.size() > 2) {
        OS = parseOS(Components[2]);
        if (Components.size() > 3) {
          Environment = parseEnvironment(Components[3]);
          ObjectFormat = parseFormat(Components[3]);
        }
      }
    } else {
      Environment =
          StringSwitch<Triple::EnvironmentType>(Components[0])
              .StartsWith("mipsn32", Triple::GNUABIN32)
              .StartsWith("mips64", Triple::GNUABI64)
              .StartsWith("mipsisa64", Triple::GNUABI64)
              .StartsWith("mipsisa32", Triple::GNU)
              .Cases("mips", "mipsel", "mipsr6", "mipsr6el", Triple::GNU)
              .Default(UnknownEnvironment);
    }
  }
  if (ObjectFormat == UnknownObjectFormat)
    ObjectFormat = getDefaultFormat(*this);
}
```

https://github.com/llvm-mirror/llvm/blob/master/include/llvm/ADT/Triple.h
```C++
  /// isMacOSX - Is this a Mac OS X triple. For legacy reasons, we support both
  /// "darwin" and "osx" as OS X triples.
  bool isMacOSX() const {
    return getOS() == Triple::Darwin || getOS() == Triple::MacOSX;
  }

 /// isOSDarwin - Is this a "Darwin" OS (OS X, iOS, or watchOS).
  bool isOSDarwin() const {
    return isMacOSX() || isiOS() || isWatchOS();
  }
```

https://github.com/llvm-mirror/llvm/blob/master/lib/Support/Host.cpp
```C++
std::string sys::getProcessTriple() {
  std::string TargetTripleString = updateTripleOSVersion(LLVM_HOST_TRIPLE);
  Triple PT(Triple::normalize(TargetTripleString));

  if (sizeof(void *) == 8 && PT.isArch32Bit())
    PT = PT.get64BitArchVariant();
  if (sizeof(void *) == 4 && PT.isArch64Bit())
    PT = PT.get32BitArchVariant();

  return PT.str();
}
```

https://github.com/llvm-mirror/llvm/blob/master/lib/Support/Unix/Host.inc
```C++
static std::string updateTripleOSVersion(std::string TargetTripleString) {
  // On darwin, we want to update the version to match that of the target.
  std::string::size_type DarwinDashIdx = TargetTripleString.find("-darwin");
  if (DarwinDashIdx != std::string::npos) {
    TargetTripleString.resize(DarwinDashIdx + strlen("-darwin"));
    TargetTripleString += getOSVersion();
    return TargetTripleString;
  }
  std::string::size_type MacOSDashIdx = TargetTripleString.find("-macos");
  if (MacOSDashIdx != std::string::npos) {
    TargetTripleString.resize(MacOSDashIdx);
    // Reset the OS to darwin as the OS version from `uname` doesn't use the
    // macOS version scheme.
    TargetTripleString += "-darwin";
    TargetTripleString += getOSVersion();
  }
  // On AIX, the AIX version and release should be that of the current host
  // unless if the version has already been specified.
  if (Triple(LLVM_HOST_TRIPLE).getOS() == Triple::AIX) {
    Triple TT(TargetTripleString);
    if (TT.getOS() == Triple::AIX && !TT.getOSMajorVersion()) {
      struct utsname name;
      if (uname(&name) != -1) {
        std::string NewOSName = Triple::getOSTypeName(Triple::AIX);
        NewOSName += name.version;
        NewOSName += '.';
        NewOSName += name.release;
        NewOSName += ".0.0";
        TT.setOSName(NewOSName);
        return TT.str();
      }
    }
  }
  return TargetTripleString;
}
```

https://github.com/llvm-mirror/llvm/blob/master/cmake/config-ix.cmake
```cmake
# By default, we target the host, but this can be overridden at CMake
# invocation time.
include(GetHostTriple)
get_host_triple(LLVM_INFERRED_HOST_TRIPLE)

set(LLVM_HOST_TRIPLE "${LLVM_INFERRED_HOST_TRIPLE}" CACHE STRING
    "Host on which LLVM binaries will run")
```

https://github.com/llvm-mirror/llvm/blob/master/cmake/modules/GetHostTriple.cmake
```cmake
# Returns the host triple.
# Invokes config.guess

function( get_host_triple var )
  if( MSVC )
    if( CMAKE_SIZEOF_VOID_P EQUAL 8 )
      set( value "x86_64-pc-windows-msvc" )
    else()
      set( value "i686-pc-windows-msvc" )
    endif()
  elseif( MINGW AND NOT MSYS )
    if( CMAKE_SIZEOF_VOID_P EQUAL 8 )
      set( value "x86_64-w64-windows-gnu" )
    else()
      set( value "i686-pc-windows-gnu" )
    endif()
  else( MSVC )
    if(CMAKE_HOST_SYSTEM_NAME STREQUAL Windows AND NOT MSYS)
      message(WARNING "unable to determine host target triple")
    else()
      set(config_guess ${LLVM_MAIN_SRC_DIR}/cmake/config.guess)
      execute_process(COMMAND sh ${config_guess}
        RESULT_VARIABLE TT_RV
        OUTPUT_VARIABLE TT_OUT
        OUTPUT_STRIP_TRAILING_WHITESPACE)
      if( NOT TT_RV EQUAL 0 )
        message(FATAL_ERROR "Failed to execute ${config_guess}")
      endif( NOT TT_RV EQUAL 0 )
      # Defer to dynamic detection of the host AIX version.
      string(REGEX REPLACE "-aix[0-9][^-]*" "-aix" value ${TT_OUT})
    endif()
  endif( MSVC )
  set( ${var} ${value} PARENT_SCOPE )
endfunction( get_host_triple var )
```

https://github.com/llvm-mirror/llvm/blob/master/cmake/config.guess
