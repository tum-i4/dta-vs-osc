# WARNING: works best for single-config generators like ninja/make and might not
#          work correctly when switching configs in multi-config generators
# coverage report generation logic heavily inspired by
# https://git.stabletec.com/other/cmake-scripts/blob/master/code-coverage.cmake
cmake_minimum_required(VERSION 3.10)

include_guard()

# options that can be enabled
option(ENABLE_COVERAGE "Enable llvm code coverage" OFF)
option(ENABLE_ASAN "Enable address sanitizer" OFF)
# setting LLVM_COV_EXCLUDE_REGEX can be used to exclude files from ccov-all
# reports (must be set before including this file)

set(COVERAGE_OUTPUT_DIR "${CMAKE_BINARY_DIR}/coverage")
file(TO_NATIVE_PATH "${COVERAGE_OUTPUT_DIR}" COVERAGE_OUTPUT_DIR_NATIVE)

# returns the cflags as string
function(get_cflags COMPILE_OPTIONS_RESULT)
  get_directory_property(ADDED_OPTIONS COMPILE_OPTIONS)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILD_TYPE_UPPER)
  set(ALL_CFLAGS "${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_${BUILD_TYPE_UPPER}} ${ADDED_OPTIONS}")
  separate_arguments(ALL_CFLAGS)

  set(${COMPILE_OPTIONS_RESULT} ${ALL_CFLAGS} PARENT_SCOPE)
endfunction()

# returns the cxxflags as string
function(get_cxxflags COMPILE_OPTIONS_RESULT)
  get_directory_property(ADDED_OPTIONS COMPILE_OPTIONS)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILD_TYPE_UPPER)
  set(ALL_CFLAGS "${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_${BUILD_TYPE_UPPER}} ${ADDED_OPTIONS}")
  separate_arguments(ALL_CFLAGS)

  set(${COMPILE_OPTIONS_RESULT} ${ALL_CFLAGS} PARENT_SCOPE)
endfunction()

# looks through the COMPILE_OPTIONS and CMAKE_CXX_FLAGS to get the last CRT flag
function (get_crt_flag RESULT_CRT_FLAG)

  # get last switch of CRT
  get_cxxflags(ALL_CFLAGS)
  string(REGEX MATCHALL "[/-](MDd?|MTd?)" MATCHED_CRTS "${ALL_CFLAGS}")
  list(GET MATCHED_CRTS -1 RELEVANT_CRT)

  set(${RESULT_CRT_FLAG} ${RELEVANT_CRT} PARENT_SCOPE)

endfunction()

# helper function that extracts lib paths from a call to clang
function(extract_lib_from_compiler LIB_BASENAME LIB_CFLAGS LIBPATH_RESULT)

  # create a dummy file we can use as input for clang
  set(DUMMY_FILE "${CMAKE_BINARY_DIR}/dummy.obj")
  file(WRITE "${DUMMY_FILE}" "dummy test file")

  get_cxxflags(CFLAGS)
  list(APPEND CFLAGS ${LIB_CFLAGS})
  list(APPEND CFLAGS "-###")
  # execute clang with the coverage flags and -### to only print the command
  # and capture the output in DUMMY_OUTPUT
  execute_process(COMMAND "${CMAKE_CXX_COMPILER}" "${DUMMY_FILE}" ${CFLAGS}
    OUTPUT_VARIABLE DUMMY_OUTPUT ERROR_VARIABLE DUMMY_ERROR)
  set(DUMMY_OUTPUT "${DUMMY_OUTPUT} ${DUMMY_ERROR}")

  # cleanup the file
  file(REMOVE "${DUMMY_FILE}")

  # asan requires more than one library so we look for libs until we cannot find more
  set(SEARCH_START 0)
  while (1)
    # parse the output for the clang_rt lib argument
    string(FIND "${DUMMY_OUTPUT}" "${LIB_BASENAME}" BASEMATCH)

    if (BASEMATCH EQUAL -1)
      # terminate search normally if we found at least one lib
      if (SEARCH_START GREATER 0)
        break()
      endif()

      message(FATAL_ERROR "Could not find ${LIB_BASENAME} in compiler output:\n\"${DUMMY_OUTPUT}\"")
    endif()

    # slice the string in two parts to find the start and end of the lib argument
    string(SUBSTRING "${DUMMY_OUTPUT}" 0 ${BASEMATCH} FRONT_STR)
    string(SUBSTRING "${DUMMY_OUTPUT}" ${BASEMATCH} -1 BACK_STR)

    string(FIND "${BACK_STR}" "\"" ENDMATCH)
    if(ENDMATCH EQUAL -1)
      message(FATAL_ERROR "Could not find quote end")
    endif()

    # lib might have been linked with wholearchive:, so check for this first
    foreach (STR IN ITEMS "-wholearchive:" "\"")
      string(FIND "${FRONT_STR}" "${STR}" STARTMATCH REVERSE)
      if (STARTMATCH EQUAL -1)
        continue()
      endif()

      # skip the matched part
      string(LENGTH "${STR}" STR_LEN)
      math(EXPR STARTMATCH "${STARTMATCH} + ${STR_LEN}")
      break()
    endforeach()

    # check for success
    if (STARTMATCH EQUAL -1)
      message(FATAL_ERROR "Could not find lib string start")
    endif()

    # extract lib from the found indices
    math(EXPR ENDMATCH "${BASEMATCH} + ${ENDMATCH}")
    math(EXPR LENGTHMATCH "${ENDMATCH} - ${STARTMATCH}")
    string(SUBSTRING "${DUMMY_OUTPUT}" ${STARTMATCH} ${LENGTHMATCH} RUNTIME_LIB)
    string(STRIP ${RUNTIME_LIB} RUNTIME_LIB)

    # sanity check that the lib file exists
    if(NOT EXISTS ${RUNTIME_LIB})
      message(FATAL_ERROR "runtime profile lib from compiler does not exist (${RUNTIME_LIB})")
    endif()

    # save the lib path
    list(APPEND RUNTIME_LIBS "${RUNTIME_LIB}")

    # set new search start and adjust output string
    math(EXPR SEARCH_START "${STARTMATCH} + ${LENGTHMATCH}")
    string(SUBSTRING "${DUMMY_OUTPUT}" ${SEARCH_START} -1 DUMMY_OUTPUT)

  endwhile()

  # set return value
  set(${LIBPATH_RESULT} ${RUNTIME_LIBS} PARENT_SCOPE)

endfunction()


# use function to get full path of requested runtime lib and enable usage
# of add_{coverage,asan} functions
function(find_runtime_lib RUNTIME_LIB LIBPATH_RESULT)

  # find out which lib is requested and set the appropriate flags for them
  set (LIB_BASENAME "clang_rt.")
  if ("profile|coverage" MATCHES "${RUNTIME_LIB}")
    set(RESULT_NAME "GLOBAL_PROFILE_LIB_PATH")
    set(CFLAGS "-fprofile-instr-generate" "-fcoverage-mapping")
  elseif("sanitizer|asan" MATCHES "${RUNTIME_LIB}")
    set(RESULT_NAME "GLOBAL_ASAN_LIB_PATH")
    set(CFLAGS "-fsanitize=address")
  else()
    message(FATAL_ERROR "Unknown argument to find_runtime_lib")
  endif()

  if (MSVC)
    # use helper to call clang and extract the lib argument
    extract_lib_from_compiler("${LIB_BASENAME}" "${CFLAGS}" RUNTIME_LIB_RESULT)
  else()
    set(RUNTIME_LIB_RESULT "${CFLAGS}")
  endif()

  # save the result for add_{coverage,asan} calls
  set(${RESULT_NAME} ${RUNTIME_LIB_RESULT} PARENT_SCOPE)

  # set return value
  set(${LIBPATH_RESULT} ${RUNTIME_LIB_RESULT} PARENT_SCOPE)

endfunction()


# creates targets to generate merged reports of all targets registered by
# target_add_coverage
function(add_code_coverage_all_targets)
  if (ENABLE_COVERAGE)
    # merge all profraw files that have been recorded in profraw.list
    add_custom_target(ccov-all-processing
      COMMAND ${LLVM_PROFDATA_PATH} merge -o ${COVERAGE_OUTPUT_DIR}/all-merged.profdata -sparse -input-files=${COVERAGE_OUTPUT_DIR}/profraw.list
    )

    if (NOT ${LLVM_COV_EXCLUDE_REGEX} STREQUAL "")
      if (NOT LLVM_COV_SUPPORTS_EXCLUDE_REGEX)
        message(WARNING "LLVM_COV_EXCLUDE_REGEX set but llvm-cov version does not support -ignore-filename-regex")
      else()
        set(exclude_regex "-ignore-filename-regex='${LLVM_COV_EXCLUDE_REGEX}'")
      endif()
    endif()

    # print a report (summary) of the merged coverage
    if (MSVC)
      add_custom_target(ccov-all-report
        COMMAND for /F \"delims=\" %i in ('type ${COVERAGE_OUTPUT_DIR_NATIVE}\\binaries.list') do ( ${LLVM_COV_PATH} report %i -instr-profile=${COVERAGE_OUTPUT_DIR}/all-merged.profdata ${exclude_regex})
        DEPENDS ccov-all-processing
      )
    else()
      add_custom_target(ccov-all-report
        COMMAND ${LLVM_COV_PATH} report "$$(" cat ${COVERAGE_OUTPUT_DIR_NATIVE}/binaries.list ")" -instr-profile=${COVERAGE_OUTPUT_DIR}/all-merged.profdata ${exclude_regex}
        DEPENDS ccov-all-processing
      )
    endif()

    # create an html report of all files
    if (MSVC)
      add_custom_target(ccov-all
        COMMAND for /F \"delims=\" %i in ('type ${COVERAGE_OUTPUT_DIR_NATIVE}\\binaries.list') do ${LLVM_COV_PATH} show %i -instr-profile=${COVERAGE_OUTPUT_DIR}/all-merged.profdata -show-line-counts-or-regions -output-dir=${COVERAGE_OUTPUT_DIR}/all-merged -format=html ${exclude_regex}
        DEPENDS ccov-all-processing
      )
    else()
      add_custom_target(ccov-all
        COMMAND ${LLVM_COV_PATH} show "$$(" cat ${COVERAGE_OUTPUT_DIR_NATIVE}/binaries.list ")" -instr-profile=${COVERAGE_OUTPUT_DIR}/all-merged.profdata -show-line-counts-or-regions -output-dir=${COVERAGE_OUTPUT_DIR}/all-merged -format=html ${exclude_regex}
        DEPENDS ccov-all-processing
      )
    endif()

    # message on where to find the report
    add_custom_command(TARGET ccov-all POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E echo "Open ${COVERAGE_OUTPUT_DIR}/all-merged/index.html to view the coverage report."
    )
  endif()
endfunction()

# convenience function to add coverage to all targets in dir and subdirs (if enabled)
# not recommended since it cannot produce a nice merged report
function(add_coverage)

  if (${ENABLE_COVERAGE})
    if (MSVC AND NOT DEFINED GLOBAL_PROFILE_LIB_PATH)
      message(FATAL_ERROR "add_coverage called but GLOBAL_PROFILE_LIB_PATH not defined")
    endif()

    add_compile_options("-fprofile-instr-generate" "-fcoverage-mapping")
    link_libraries("${GLOBAL_PROFILE_LIB_PATH}")

  endif()
endfunction()

# convenience function to set compiler flags and link lib
function(target_add_coverage target)
  # Define the supported set of keywords
  set(prefix ARG)
  set(noValues is_test)
  # set(singleValues target)
  # set(multiValues SOURCES IMAGES)
  # Process the arguments passed in
  cmake_parse_arguments(
    ${prefix}
    "${noValues}"
    "${singleValues}"
    "${multiValues}"
    ${ARGN}
  )

  if (${ENABLE_COVERAGE})

    if (MSVC AND NOT DEFINED GLOBAL_PROFILE_LIB_PATH)
      message(FATAL_ERROR "add_coverage called but GLOBAL_PROFILE_LIB_PATH not defined")
    endif()

    # we generate coverage by instrumenting unit tests but obviously don't care
    # about coverage of the unit test code itself
    if (NOT ${ARG_is_test})
      target_compile_options(${target}
        PRIVATE "-fprofile-instr-generate" "-fcoverage-mapping"
      )
      target_link_libraries(${target} PRIVATE "${GLOBAL_PROFILE_LIB_PATH}")
    endif()

    get_target_property(target_type ${target} TYPE)
    if (target_type STREQUAL "EXECUTABLE")
      if (NOT ${LLVM_COV_EXCLUDE_REGEX} STREQUAL "")
        if (NOT LLVM_COV_SUPPORTS_EXCLUDE_REGEX)
          message(WARNING "LLVM_COV_EXCLUDE_REGEX set but llvm-cov version does not support -ignore-filename-regex")
        else()
          set(exclude_regex "-ignore-filename-regex='${LLVM_COV_EXCLUDE_REGEX}'")
        endif()
      endif()
      # run the binary, generate profraw data and put target into binaries.list
      # for consolidated report
      if (MSVC)
        add_custom_target(ccov-run-${target}
          # COMMAND set \"LLVM_PROFILE_FILE=${target}.profraw\" && $<TARGET_FILE:${target}>
          COMMAND ${CMAKE_COMMAND} -E env "LLVM_PROFILE_FILE=${target}.profraw" $<TARGET_FILE:${target}>
          COMMAND ${CMAKE_COMMAND} -E echo_append "-object=$<TARGET_FILE:${target}> " >> ${COVERAGE_OUTPUT_DIR_NATIVE}\\binaries.list
          COMMAND echo ${CMAKE_CURRENT_BINARY_DIR}/${target}.profraw >> ${COVERAGE_OUTPUT_DIR_NATIVE}\\profraw.list
          DEPENDS ccov-create-dir ${target}
        )
      else()
        add_custom_target(ccov-run-${target}
          # COMMAND "LLVM_PROFILE_FILE=${target}.profraw" $<TARGET_FILE:${target}>
          # COMMAND echo -n \"-object=$<TARGET_FILE:${target}> \" >> ${COVERAGE_OUTPUT_DIR_NATIVE}/binaries.list
          COMMAND ${CMAKE_COMMAND} -E echo_append "-object=$<TARGET_FILE:${target}> " >> ${COVERAGE_OUTPUT_DIR_NATIVE}/binaries.list
          COMMAND ${CMAKE_COMMAND} -E env "LLVM_PROFILE_FILE=${target}.profraw" $<TARGET_FILE:${target}>
          COMMAND echo ${CMAKE_CURRENT_BINARY_DIR}/${target}.profraw >> ${COVERAGE_OUTPUT_DIR_NATIVE}/profraw.list
          DEPENDS ccov-create-dir ${target}
        )
      endif()

      # create profdata from profraw files
      add_custom_target(ccov-processing-${target}
        COMMAND ${LLVM_PROFDATA_PATH} merge -sparse ${target}.profraw -o ${target}.profdata
        DEPENDS ccov-run-${target}
      )

      # print source code with annotated coverage
      add_custom_target(ccov-show-${target}
        COMMAND ${LLVM_COV_PATH} show $<TARGET_FILE:${target}> -instr-profile=${target}.profdata -show-line-counts-or-regions ${exclude_regex}
        DEPENDS ccov-processing-${target}
      )

      # print simple coverage report
      add_custom_target(ccov-report-${target}
        COMMAND ${LLVM_COV_PATH} report $<TARGET_FILE:${target}> -instr-profile=${target}.profdata ${exclude_regex}
        DEPENDS ccov-processing-${target}
      )

      # generate html report from profdata and store it in coverage dir
      add_custom_target(ccov-${target}
        COMMAND ${LLVM_COV_PATH} show $<TARGET_FILE:${target}> -instr-profile=${target}.profdata -show-line-counts-or-regions -output-dir=${COVERAGE_OUTPUT_DIR}/${target} -format=html ${exclude_regex}
        DEPENDS ccov-processing-${target}
      )

      # print message on where to find the coverage report
      add_custom_command(TARGET ccov-${target} POST_BUILD
        COMMAND ;
        COMMENT "Open ${COVERAGE_OUTPUT_DIR}/${target}/index.html to view coverage report"
      )

      # add ccov target that builds all coverage reports individually
      if (NOT TARGET ccov)
        add_custom_target(ccov)
      endif()
      add_dependencies(ccov ccov-${target})

      # add link between ccov-all and specific target
      add_dependencies(ccov-all-processing ccov-run-${target})

    endif()

  endif()
endfunction()


# convenience function to add asan to all targets in dir and subdirs (if enabled)
function(add_asan)

  if (${ENABLE_ASAN})

    if (NOT DEFINED GLOBAL_ASAN_LIB_PATH)
      message(FATAL_ERROR "add_asan called but GLOBAL_ASAN_LIB_PATH not defined")
    endif()

    add_compile_options("-fsanitize=address")
    link_libraries("${GLOBAL_ASAN_LIB_PATH}")

  endif()

endfunction()

# convenience function to set compiler flags and link lib
function(target_add_asan target)

  if (${ENABLE_ASAN})

    if (NOT DEFINED GLOBAL_ASAN_LIB_PATH)
      message(FATAL_ERROR "add_asan called but GLOBAL_ASAN_LIB_PATH not defined")
    endif()

    target_compile_options(${target}
      PRIVATE "-fsanitize=address"
    )
    target_link_libraries(${target} PRIVATE "${GLOBAL_ASAN_LIB_PATH}")

  endif()
endfunction()

# checks if coverage is enabled, the CRT is supported and finds the library path
if (ENABLE_COVERAGE)
  if (NOT "${CMAKE_CXX_COMPILER}" MATCHES "clang")
    message(FATAL_ERROR "ENABLE_COVERAGE is only supported for clang")
  endif()

  # check for invalid flags
  if (MSVC)
    get_crt_flag(CRT_FLAG)
    if ("${CRT_FLAG}" MATCHES "([-/])MD")
      set(NEW_CRT "${CMAKE_MATCH_1}MT")
      message(WARNING "COVERAGE only supports statically linking CRT\n"
        "Detected flag: ${CRT_FLAG}\n"
        "Adding compiler flag ${NEW_CRT} to override CRT")
      add_compile_options("${NEW_CRT}")
    endif()
  endif()

  find_runtime_lib("coverage" PROFILE_LIB)
  message(STATUS "PROFILE_LIB: ${PROFILE_LIB}")

  # add target to make sure the coverage output dir exists
  add_custom_target(ccov-create-dir
    COMMAND ${CMAKE_COMMAND} -E make_directory ${COVERAGE_OUTPUT_DIR}
    DEPENDS ccov-clean
  )

  # clean artifacts from previous builds
  add_custom_target(ccov-clean
    COMMAND ${CMAKE_COMMAND} -E remove ${COVERAGE_OUTPUT_DIR}/binaries.list
    COMMAND ${CMAKE_COMMAND} -E remove ${COVERAGE_OUTPUT_DIR}/profraw.list
  )
  
  # check for required programs for report generation
  find_program(LLVM_COV_PATH llvm-cov)
  if (NOT LLVM_COV_PATH)
    message(FATAL_ERROR "Could not find llvm-cov")
  endif()

  execute_process(COMMAND ${LLVM_COV_PATH} --version
    OUTPUT_VARIABLE LLVM_COV_VERSION_CALL_OUTPUT)
  string(REGEX MATCH
    "[0-9]+\\.[0-9]+\\.[0-9]+"
    LLVM_COV_VERSION
    ${LLVM_COV_VERSION_CALL_OUTPUT})

  if(LLVM_COV_VERSION VERSION_LESS "7.0.0")
    set(LLVM_COV_SUPPORTS_EXCLUDE_REGEX 0)
  else()
    set(LLVM_COV_SUPPORTS_EXCLUDE_REGEX 1)
  endif()

  find_program(LLVM_PROFDATA_PATH llvm-profdata)
  if (NOT LLVM_PROFDATA_PATH)
    message(FATAL_ERROR "Could not find llvm-profdata")
  endif()

  # create target where target_add_coverage targets are registered
  add_code_coverage_all_targets()
endif()

# checks if asan is enabled, the CRT is supported and finds the library path
if (ENABLE_ASAN)
  if (NOT "${CMAKE_CXX_COMPILER}" MATCHES "clang")
    message(FATAL_ERROR "ENABLE_ASAN is only supported for clang")
  endif()

  if (MSVC)
    # check for invalid flags
    get_crt_flag(CRT_FLAG)
    if ("${CRT_FLAG}" MATCHES "([-/]M[DT])d")
      set(NEW_CRT ${CMAKE_MATCH_1})
      message(WARNING "ASAN does not support debug CRTs /MDd or /MTd\n"
        "Detected flag: ${CRT_FLAG}\n"
        "Adding compiler flag ${NEW_CRT} to override CRT")
      add_compile_options("${NEW_CRT}")
    endif()
  endif()

  find_runtime_lib("asan" ASAN_LIB)
  message(STATUS "ASAN_LIB: ${ASAN_LIB}")
endif()
