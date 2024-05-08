import redis

# Connect to Redis
redis_client = redis.StrictRedis(host='localhost', port=6379, db=0)

# Example: Adding a key-value pair
redis_client.set('my_key', 'my_value')

# Example: Adding a hash
redis_client.hset('my_hash', 'field1', 'value1')
redis_client.hset('my_hash', 'field2', 'value2')

# Example: Adding items to a list
redis_client.rpush('my_list', 'item1')
redis_client.rpush('my_list', 'item2')
redis_client.rpush('my_list', 'item3')

# Example: Adding items to a set
redis_client.sadd('my_set', 'item1')
redis_client.sadd('my_set', 'item2')
redis_client.sadd('my_set', 'item3')

# Example: Adding items to a sorted set
redis_client.zadd('my_sorted_set', {'item1': 1, 'item2': 2, 'item3': 3})




# Function to retrieve all keys in the database
def get_all_keys():
    return redis_client.keys()

# Function to retrieve all data associated with a key
def get_data_for_key(key):
    key_type = redis_client.type(key).decode('utf-8')
    if key_type == 'string':
        return redis_client.get(key).decode('utf-8')
    elif key_type == 'hash':
        return redis_client.hgetall(key)
    elif key_type == 'list':
        return redis_client.lrange(key, 0, -1)
    elif key_type == 'set':
        return redis_client.smembers(key)
    elif key_type == 'zset':
        return redis_client.zrange(key, 0, -1, withscores=True)
    else:
        return "Unknown data type"

# Get all keys in the database
keys = get_all_keys()

# Display all information
for key in keys:
    data = get_data_for_key(key)
    print(f"Key: {key.decode('utf-8')}, Type: {redis_client.type(key).decode('utf-8')}, Data: {data}")

