import jwt
import datetime

# 1. Configuration
SECRET_KEY = "your_super_secret_random_string"
ALGORITHM = "HS256"


def create_token(user_id):
    """Generates a token that expires in 1 hour."""
    payload = {
        "user_id": user_id,
        "exp": datetime.datetime.now(datetime.timezone.utc)
        + datetime.timedelta(hours=1),
        "iat": datetime.datetime.now(datetime.timezone.utc),
    }
    return jwt.encode(payload, SECRET_KEY, algorithm=ALGORITHM)


def verify_token(token):
    """Decodes the token and checks for validity/expiration."""
    try:
        decoded_payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
        return decoded_payload
    except jwt.ExpiredSignatureError:
        return "Error: Token has expired."
    except jwt.InvalidTokenError:
        return "Error: Invalid token."


# --- Testing the prototype ---
if __name__ == "__main__":
    # Create it
    my_token = create_token(user_id=123)
    print(f"Generated JWT: {my_token}\n")

    # Verify it
    result = verify_token(my_token)
    print(f"Decoded Result: {result}")
