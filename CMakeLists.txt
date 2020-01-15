cmake_minimum_required (VERSION 3.8)
project (calamity VERSION "1.0")

configure_file(config.h.in config.h)

# pkg-config setup
find_package(PkgConfig)

# Search for Amino
pkg_search_module(AMINO REQUIRED amino)
pkg_search_module(SNS REQUIRED sns)
pkg_search_module(ACH REQUIRED ach)


set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_executable( sns_tmsmt_coms src/sns-tmsmt-coms.c)
target_include_directories( sns_tmsmt_coms PUBLIC ${CMAKE_CURRENT_BINARY_DIR}
    ${AMINO_INCLUDE_DIRS}  ${ACH_INCLUDE_DIRS} ${SNS_INCLUDE_DIRS}
    )
  target_link_libraries( sns_tmsmt_coms
    ${AMINO_LIBRARIES} ${ACH_LIBRARIES} ${SNS_LIBRARIES} -ltmsmt
    )
message("${AMINO_LIBRARIES} ${ACH_LIBRARIES} ${SNS_LIBRARIES} -ltmsmt")
install(TARGETS sns_tmsmt_coms
 RUNTIME DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})


# uninstall target
if(NOT TARGET uninstall)
    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
        IMMEDIATE @ONLY)

    add_custom_target(uninstall
        COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
endif()
