from pydantic import BaseModel, Field
from strands import Agent
from strands.models.ollama import OllamaModel

# 1. Define a strict Pydantic model to guarantee an integer return
class CalorieAnalysis(BaseModel):
    """Container for the calorie estimation results."""
    calories: int = Field(description="The total estimated calories as a single integer number.")
    food_type: str = Field(description="The name of the food")

# 2. Initialize the Ollama model via Strands
ollama_model = OllamaModel(
    host="http://localhost:11434",
    model_id="gemma4:26b",
    temperature=0.0  # Set to 0.0 for absolute precision and constraint adherence
)

# 3. Create the Strands agent
agent = Agent(model=ollama_model)

image_path = '1.jpg'

# 4. Invoke the agent with the text prompt and the structured output model
result = agent(
    f"Analyze the image at path '{image_path}' and tell me how many calories this burger has.",
    structured_output_model=CalorieAnalysis,
)

# 5. Extract the fields from the structured output
burger_calories: int = result.structured_output.calories
food_name: str = result.structured_output.food_type

# Print both the food name and the calories
print(f"Food: {food_name}")
print(f"Calories: {burger_calories}")