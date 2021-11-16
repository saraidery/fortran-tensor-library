
macro(add_ftensor_unittest _name _labels)

   set (filename unit_${_name})

   add_pfunit_ctest (${filename}
      TEST_SOURCES ${CMAKE_BINARY_DIR}/${filename}.pf
      LINK_LIBRARIES ftensor_library
   )

   # add "unit-test" as a label for all unit tests
   string(CONCAT labels "${_labels}" "; unit-test")

   set_tests_properties(${filename} PROPERTIES LABELS "${labels}")

endmacro()

#add_ftensor_unittest(warning_suppressor "dummy; warning-suppression")
