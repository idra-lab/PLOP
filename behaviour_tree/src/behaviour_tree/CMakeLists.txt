cmake_minimum_required(VERSION 3.18)

project(behaviour_tree LANGUAGES CXX)

include(FetchContent)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)


find_package(ament_cmake QUIET)

# Either colcon or cmake, please no ROS1
if(ament_cmake_FOUND)
    set(THIS_PACKAGE_DEPS
        rclcpp
        rclcpp_action
        behaviortree_cpp
        behaviortree_ros2
        action_tutorials_interfaces
    )

    foreach(PACKAGE ${THIS_PACKAGE_DEPS})
        find_package(${PACKAGE} REQUIRED )
    endforeach()

    add_executable(behaviour_tree src/main.cpp)

    ament_target_dependencies(behaviour_tree ${THIS_PACKAGE_DEPS})

    target_include_directories(behaviour_tree PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>)
    target_compile_features(behaviour_tree PUBLIC c_std_99 cxx_std_17)

    install(TARGETS behaviour_tree DESTINATION lib/${PROJECT_NAME})

    ament_package()

else()

    message(FATAL_ERROR "This package is meant to be built with ROS2")
    
    add_executable(behaviour_tree src/main.cpp)

    FetchContent_Declare(
        behaviortree_cpp
        GIT_REPOSITORY "https://github.com/BehaviorTree/BehaviorTree.CPP.git"
        GIT_TAG "tags/4.6.0"  
        SOURCE_DIR "${CMAKE_CURRENT_LIST_DIR}/../BehaviourTree.CPP"
    )
    FetchContent_MakeAvailable(behaviortree_cpp)

    target_link_libraries(behaviour_tree BT::behaviortree_cpp)

endif()
