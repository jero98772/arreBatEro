import { useEffect, useState, useRef } from "react";

function App() {
  const [ws, setWs] = useState(null);
  const [messages, setMessages] = useState([]);
  const [input, setInput] = useState("");
  const messagesEndRef = useRef(null);

  useEffect(() => {
  const websocket = new WebSocket("ws://localhost:8000/ws");

  websocket.onopen = () => {
    console.log("WebSocket connected!");
  };

  websocket.onerror = (error) => {
    console.error("WebSocket error:", error);
  };

  websocket.onmessage = (event) => {
    const data = JSON.parse(event.data);

    setMessages((prev) => {
      if (data.role === "user") {
        return [...prev, { role: "user", content: data.content }];
      } else if (data.role === "ai") {
        return [...prev, { role: "ai", content: data.content }];
      } else if (data.role === "ai_stream") {
        const lastMessage = prev[prev.length - 1];
        if (lastMessage && lastMessage.role === "ai") {
          lastMessage.content += data.content;
          return [...prev.slice(0, -1), lastMessage];
        }
        return [...prev, { role: "ai", content: data.content }];
      } else if (data.role === "ai_final") {
        const lastMessage = prev[prev.length - 1];
        if (lastMessage && lastMessage.role === "ai") {
          lastMessage.content = data.content;
          return [...prev.slice(0, -1), lastMessage];
        }
        return prev;
      }
      return prev;
    });
  };

  setWs(websocket);

  return () => {
    websocket.close();
  };
}, []);


  const sendMessage = () => {
    if (!ws || ws.readyState !== WebSocket.OPEN) {
      console.error("WebSocket is not open. Message not sent.");
      return;
    }

    if (input.trim() !== "") {
      ws.send(input);
      setInput("");
    }
  };

  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages]);

  return (
    <div className="p-4 max-w-2xl mx-auto">
      <h1 className="text-2xl font-bold text-center mb-4">Chat Assistant</h1>
      <div className="border p-4 rounded h-96 overflow-y-auto">
        {messages.map((msg, i) => (
          <div
            key={i}
            className={`p-2 rounded mb-2 ${
              msg.role === "user"
                ? "bg-blue-500 text-white self-end"
                : "bg-gray-200 text-black self-start"
            }`}
          >
            {msg.content}
          </div>
        ))}
        <div ref={messagesEndRef} />
      </div>
      <div className="mt-4 flex">
        <input
          type="text"
          className="border p-2 flex-1 rounded"
          value={input}
          onChange={(e) => setInput(e.target.value)}
          onKeyDown={(e) => e.key === "Enter" && sendMessage()}
        />
        <button
          className="ml-2 px-4 py-2 bg-blue-500 text-white rounded"
          onClick={sendMessage}
        >
          Send
        </button>
      </div>
    </div>
  );
}

export default App;
