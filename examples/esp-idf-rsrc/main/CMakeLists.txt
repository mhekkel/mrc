find_package(Mrc)

idf_component_register(SRCS esp-rsrc-app.cpp
                       INCLUDE_DIRS ".")

mrc_write_header(${CMAKE_CURRENT_SOURCE_DIR}/mrsrc.hpp)

mrc_target_resources(${COMPONENT_LIB} CREATE_ELF_TEMPLATE
	RESOURCES ${CMAKE_CURRENT_SOURCE_DIR}/hello.txt)
