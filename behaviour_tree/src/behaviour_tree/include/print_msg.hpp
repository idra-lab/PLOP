#pragma once

#include <behaviortree_ros2/bt_action_node.hpp>

class PrintMsg : public BT::SyncActionNode
{
public:
  PrintMsg(const std::string& name, const BT::NodeConfig& config)
    : BT::SyncActionNode(name, config)
  {}

  BT::NodeStatus tick() override
  {
    std::string msg;
    if(getInput("message", msg))
    {
      std::cout << "PrintMsg: " << msg << std::endl;
      return BT::NodeStatus::SUCCESS;
    }
    else
    {
      std::cout << "PrintMsg FAILED " << std::endl;
      return BT::NodeStatus::FAILURE;
    }
  }

  static BT::PortsList providedPorts()
  {
    return { BT::InputPort<std::string>("message") };
  }
};