const { useState } = React;

function App() {
    const [items, setItems] = useState(window.initialItems || []);
    
    return (
        <div className="container">
            <h1>My Todo List</h1>
            
            <form action="/add" method="POST">
                <input 
                    type="text" 
                    name="item"
                    placeholder="Add new item..."
                    required
                />
                <button type="submit">Add</button>
            </form>
            
            <ul>
                {items.map((item, index) => (
                    <li key={index}>
                        {item}
                        <form action={`/delete/${index}`} method="POST" style={{display: 'inline'}}>
                            <button type="submit">Delete</button>
                        </form>
                    </li>
                ))}
            </ul>
        </div>
    );
}

ReactDOM.render(<App />, document.getElementById('root'));