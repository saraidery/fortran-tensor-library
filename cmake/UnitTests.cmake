
macro(add_ftensor_unittest _name _labels)

   set (filename ${_name})

   file (COPY ${CMAKE_SOURCE_DIR}/tests/${_name}.pf 
         DESTINATION ${CMAKE_BINARY_DIR})

   add_pfunit_ctest (${filename}
      TEST_SOURCES ${CMAKE_BINARY_DIR}/${filename}.pf
      LINK_LIBRARIES ftensor_library ${EXTRA_LINKER_FLAGS}
   )

   set_tests_properties(${filename} PROPERTIES LABELS "${labels}")

endmacro()

add_ftensor_unittest(create_tensor "tensor; constructor")
