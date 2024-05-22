#include <action_tutorials_interfaces/action/fibonacci.hpp>
#include <rclcpp/rclcpp.hpp>
#include <rclcpp_action/rclcpp_action.hpp>

#include <behaviortree_ros2/bt_action_node.hpp>
#include "behaviortree_ros2/plugins.hpp"

#include <print_msg.hpp>

#include <cstdlib>

using namespace BT;

// let's define these, for brevity
using Fibonacci = action_tutorials_interfaces::action::Fibonacci;
using GoalHandleFibonacci = rclcpp_action::ServerGoalHandle<Fibonacci>;

class FibonacciAction: public RosActionNode<Fibonacci>
{
public:
  FibonacciAction(const std::string& name,
                  const NodeConfig& conf,
                  const RosNodeParams& params)
    : RosActionNode<Fibonacci>(name, conf, params)
  {}

  static PortsList providedPorts()
  {
    return providedBasicPorts({InputPort<unsigned>("order")});
  }

  // This is called when the TreeNode is ticked and it should
  // send the request to the action server
  bool setGoal(RosActionNode::Goal& goal) override 
  {
    system("ros2 action send_goal /fibonacci action_tutorials_interfaces/action/Fibonacci \"{order: 5}\" &");
    return true;
  }
  
  // Callback executed when the reply is received.
  // Based on the reply you may decide to return SUCCESS or FAILURE.
  NodeStatus onResultReceived(const WrappedResult& wr) override
  {
    std::stringstream ss;
    ss << "Result received: ";
    for (auto number : wr.result->sequence) {
      ss << number << " ";
    }
    std::cout << ss.str() << std::endl;
    // RCLCPP_INFO(node_->get_logger(), ss.str().c_str());
    return NodeStatus::SUCCESS;
  }

  // Callback invoked when there was an error at the level
  // of the communication between client and server.
  // This will set the status of the TreeNode to either SUCCESS or FAILURE,
  // based on the return value.
  // If not overridden, it will return FAILURE by default.
  virtual NodeStatus onFailure(ActionNodeErrorCode error) override
  {
    std::cerr << "Error: " << error << std::endl;
    // RCLCPP_ERROR(node_->get_logger(), "Error: %d", error);
    return NodeStatus::FAILURE;
  }

  // we also support a callback for the feedback, as in
  // the original tutorial.
  // Usually, this callback should return RUNNING, but you
  // might decide, based on the value of the feedback, to abort
  // the action, and consider the TreeNode completed.
  // In that case, return SUCCESS or FAILURE.
  // The Cancel request will be send automatically to the server.
  NodeStatus onFeedback(const std::shared_ptr<const Feedback> feedback)
  {
    std::stringstream ss;
    ss << "Next number in sequence received: ";
    for (auto number : feedback->partial_sequence) {
      ss << number << " ";
    }
    std::cout << ss.str() << std::endl;
    // RCLCPP_INFO(node_->get_logger(), ss.str().c_str());
    return NodeStatus::RUNNING;
  }
};

static const char* xml_text = R"(
  <root BTCPP_format="4">
    <BehaviorTree>
      <Sequence>
        <PrintMsg message="start"/>
        <Fibonacci name="FibonacciA" order="5"/>
        <PrintMsg message="Fibonacci completed"/>
      </Sequence>
    </BehaviorTree>
  </root>
)";


int main (int argc, char ** argv){
  rclcpp::init(argc, argv);
  auto node = std::make_shared<rclcpp::Node>("fibonacci_action_client");

  BehaviorTreeFactory factory;

  factory.registerNodeType<PrintMsg>("PrintMsg");

  // provide the ROS node and the name of the action service
  RosNodeParams params; 
  params.nh = node;
  params.default_port_value = "fibonacci";
  factory.registerNodeType<FibonacciAction>("Fibonacci", params);


  auto tree = factory.createTreeFromText(xml_text);

  for(int i = 0; i < 5; i++)
  {
    tree.tickWhileRunning();
  }

  return 0;
}