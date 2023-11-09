import React, { useEffect, useState } from 'react';

function App() {
  const [data, setData] = useState('');

  useEffect(() => {
    fetchData();
  }, []);

  const fetchData = async () => {
    try {
      const response = await fetch('/api/data'); // Fetch data from your Flask API
      const result = await response.json();
      setData(result.message); // Set the received data to state
    } catch (error) {
      console.error('Error fetching data:', error);
    }
  };

  return (
    <div className="App">
      <h1>Data from Flask API:</h1>
      <p>{data}</p>
    </div>
  );
}

export default App;
