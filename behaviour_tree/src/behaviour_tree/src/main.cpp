#include "behaviortree_cpp/bt_factory.h"
#include <memory>

using namespace BT;

// clang-format off
static const char* xml_text = R"(

 <root BTCPP_format="4" >

     <BehaviorTree ID="MainTree">
        <Sequence name="root">
            <AlwaysSuccess/>
            <ROS2Message   message="this works too" />
            <ROS2Message   message="{the_answer}" />
        </Sequence>
     </BehaviorTree>

 </root>
 )";
// clang-format on

class ROS2Message : public BT::SyncActionNode
{
  public:
  ROS2Message(const std::string& name, const BT::NodeConfig& config) :
        BT::SyncActionNode(name, config)
  {}

  BT::NodeStatus tick() override
  {
    std::string msg;
    getInput("message", msg);
    std::cout << msg << std::endl;
    return BT::NodeStatus::SUCCESS;
  }

  static BT::PortsList providedPorts()
  {
    return {BT::InputPort<std::string>("message")};
  }
};

int main()
{
  BehaviorTreeFactory factory;

  factory.registerNodeType<ROS2Message>("ROS2Message");
  std::unique_ptr<BT::Tree> tree = nullptr;
  int i = 0;
  while (i<10){
    if (tree == nullptr || BT::isStatusCompleted(tree->tickExactlyOnce())){
      tree = std::make_unique<BT::Tree>(factory.createTreeFromText(xml_text));
      tree->tickWhileRunning();
      i++;
    }
  }
  return 0;
}
