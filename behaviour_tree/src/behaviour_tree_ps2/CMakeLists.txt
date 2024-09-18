cmake_minimum_required(VERSION 3.8)
project(behaviour_tree_ps2)

if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  add_compile_options(-Wall -Wextra -Wpedantic)
endif()

find_package(ament_cmake REQUIRED)

set(THIS_PACKAGE_DEPS
  rclcpp
  plansys2_planner
)

foreach(DEP ${THIS_PACKAGE_DEPS})
  find_package(${DEP} REQUIRED)
endforeach()

add_executable(behaviour_tree_node src/behaviour_tree_node.cpp)

ament_target_dependencies(behaviour_tree_node ${THIS_PACKAGE_DEPS})

target_include_directories(behaviour_tree_node PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>)
target_compile_features(behaviour_tree_node PUBLIC c_std_99 cxx_std_17)  # Require C99 and C++17

install(TARGETS behaviour_tree_node
  DESTINATION lib/${PROJECT_NAME})

if(BUILD_TESTING)
  find_package(ament_lint_auto REQUIRED)
  # the following line skips the linter which checks for copyrights
  # comment the line when a copyright and license is added to all source files
  set(ament_cmake_copyright_FOUND TRUE)
  # the following line skips cpplint (only works in a git repo)
  # comment the line when this package is in a git repo and when
  # a copyright and license is added to all source files
  set(ament_cmake_cpplint_FOUND TRUE)
  ament_lint_auto_find_test_dependencies()
endif()

ament_package()
