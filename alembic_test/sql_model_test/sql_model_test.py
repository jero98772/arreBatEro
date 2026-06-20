import bcrypt
import psycopg2
from typing import List, Optional
from sqlmodel import Field, SQLModel, Session, create_engine, select
from psycopg2.extensions import ISOLATION_LEVEL_AUTOCOMMIT


def create_database_if_not_exists():
    # Connect to the default 'postgres' db to run CREATE DATABASE
    conn = psycopg2.connect(
        dbname="postgres",
        user="postgres",
        password="postgres",
        host="localhost",
        port=5432,
    )
    conn.set_isolation_level(ISOLATION_LEVEL_AUTOCOMMIT)
    cursor = conn.cursor()

    cursor.execute("SELECT 1 FROM pg_database WHERE datname = 'testdb_sql_model2'")
    if not cursor.fetchone():
        cursor.execute("CREATE DATABASE testdb_sql_model2")
        print("Database 'testdb_sql_model' created.")
    else:
        print("Database already exists.")

    cursor.close()
    conn.close()


# 1. Database Setup
# Replace with your actual PostgreSQL credentials
DATABASE_URL = "postgresql://postgres:postgres@localhost:5432/testdb_sql_model2"
engine = create_engine(DATABASE_URL, echo=True)


# 2. Database Model
class User(SQLModel, table=True):
    __tablename__: str = "users"

    id: Optional[int] = Field(default=None, primary_key=True)
    username: str = Field(unique=True, index=True, nullable=False)
    hashed_password: str = Field(nullable=False)
    description: Optional[str] = Field(default=None, nullable=True)  # ← add this


# 3. Password Hashing Helpers
def hash_password(password: str) -> str:
    salt = bcrypt.gensalt()
    return bcrypt.hashpw(password.encode("utf-8"), salt).decode("utf-8")


def verify_password(password: str, hashed_password: str) -> bool:
    return bcrypt.checkpw(password.encode("utf-8"), hashed_password.encode("utf-8"))


# 4. CRUD Operations
def create_user(username: str, password: str) -> User:
    hashed = hash_password(password)
    db_user = User(username=username, hashed_password=hashed)
    with Session(engine) as session:
        session.add(db_user)
        session.commit()
        session.refresh(db_user)
        return db_user


def get_user_by_username(username: str) -> Optional[User]:
    with Session(engine) as session:
        statement = select(User).where(User.username == username)
        return session.exec(statement).first()


def get_all_users() -> List[User]:
    with Session(engine) as session:
        statement = select(User)
        return session.exec(statement).all()


def update_user_password(username: str, new_password: str) -> Optional[User]:
    with Session(engine) as session:
        statement = select(User).where(User.username == username)
        db_user = session.exec(statement).first()
        if db_user:
            db_user.hashed_password = hash_password(new_password)
            session.add(db_user)
            session.commit()
            session.refresh(db_user)
            return db_user
        return None


def delete_user(username: str) -> bool:
    with Session(engine) as session:
        statement = select(User).where(User.username == username)
        db_user = session.exec(statement).first()
        if db_user:
            session.delete(db_user)
            session.commit()
            return True
        return False


# 5. Example Execution Workflow
if __name__ == "__main__":
    # Standard SQLModel metadata creation (if not using Alembic migrations right away)
    create_database_if_not_exists()
    SQLModel.metadata.create_all(engine)

    print("--- Creating User ---")
    user = create_user("neo_matrix", "FollowTheWhiteRabbit")
    user2 = create_user("neo_matrix2", "FollowTheWhiteRabbit2")

    print(f"Created: {user.username} with ID {user.id}")

    print("\n--- Reading User ---")
    fetched_user = get_user_by_username("neo_matrix")
    if fetched_user:
        print(f"Found User: {fetched_user.username}")
        print(
            f"Password Verify: {verify_password('FollowTheWhiteRabbit', fetched_user.hashed_password)}"
        )

    print("\n--- Updating Password ---")
    update_user_password("neo_matrix", "ThereIsNoSpoon")

    print("\n--- Verifying Updated Password ---")
    updated_user = get_user_by_username("neo_matrix")
    if updated_user:
        print(
            f"Old Password valid?: {verify_password('FollowTheWhiteRabbit', updated_user.hashed_password)}"
        )
        print(
            f"New Password valid?: {verify_password('ThereIsNoSpoon', updated_user.hashed_password)}"
        )

    print("\n--- Deleting User ---")
    if delete_user("neo_matrix"):
        print("User deleted successfully.")
