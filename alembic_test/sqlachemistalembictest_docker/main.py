import bcrypt
from typing import List, Optional
from sqlalchemy import create_engine, String, select, text  # Added text here
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column, sessionmaker

# 1. Database Setup
DATABASE_URL = "postgresql://postgres:postgres@localhost:5433/my_db_sql_alch"
engine = create_engine(DATABASE_URL, echo=True)
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)


# 2. Database Model Base and Definitions
class Base(DeclarativeBase):
    pass


class User(Base):
    __tablename__ = "users"

    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    username: Mapped[str] = mapped_column(
        String(50), unique=True, index=True, nullable=False
    )
    hashed_password: Mapped[str] = mapped_column(String(255), nullable=False)


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
    with SessionLocal() as session:
        session.add(db_user)
        session.commit()
        session.refresh(db_user)
        return db_user


def get_user_by_username(username: str) -> Optional[User]:
    with SessionLocal() as session:
        statement = select(User).where(User.username == username)
        return session.scalars(statement).first()


def get_all_users() -> List[User]:
    with SessionLocal() as session:
        statement = select(User)
        return list(session.scalars(statement).all())


def update_user_password(username: str, new_password: str) -> Optional[User]:
    with SessionLocal() as session:
        statement = select(User).where(User.username == username)
        db_user = session.scalars(statement).first()
        if db_user:
            db_user.hashed_password = hash_password(new_password)
            session.commit()
            session.refresh(db_user)
            return db_user
        return None


def delete_user(username: str) -> bool:
    with SessionLocal() as session:
        statement = select(User).where(User.username == username)
        db_user = session.scalars(statement).first()
        if db_user:
            session.delete(db_user)
            session.commit()
            return True
        return False


def create_database_if_not_exists():
    # Connect to 'postgres' (the default DB that always exists)
    default_url = DATABASE_URL.rsplit("/", 1)[0] + "/postgres"
    tmp_engine = create_engine(default_url, isolation_level="AUTOCOMMIT")

    with tmp_engine.connect() as conn:
        result = conn.execute(
            text("SELECT 1 FROM pg_database WHERE datname='my_db_sql_alch'")
        )
        exists = result.scalar()

        if not exists:
            print("Database 'my_db_sql_alch' not found. Creating it now...")
            conn.execute(text("CREATE DATABASE my_db_sql_alch"))
            print("Database created successfully!")


# 5. Example Execution Workflow
if __name__ == "__main__":
    # Create tables programmatically if not running Alembic migrations immediately
    create_database_if_not_exists()

    Base.metadata.create_all(engine)

    print("--- Creating User ---")
    user = create_user("trinity_matrix", "DodgeThis123")
    user2 = create_user("trinity_matrix2", "DodgeThis1232")

    print(f"Created: {user.username} with ID {user.id}")

    print("\n--- Reading User ---")
    fetched_user = get_user_by_username("trinity_matrix")
    if fetched_user:
        print(f"Found User: {fetched_user.username}")
        print(
            f"Password Verify: {verify_password('DodgeThis123', fetched_user.hashed_password)}"
        )
    print("\n--- Updating Password ---")
    update_user_password("trinity_matrix", "MorpheusIsFree")

    print("\n--- Verifying Updated Password ---")
    updated_user = get_user_by_username("trinity_matrix")
    if updated_user:
        print(
            f"Old Password valid?: {verify_password('DodgeThis123', updated_user.hashed_password)}"
        )
        print(
            f"New Password valid?: {verify_password('MorpheusIsFree', updated_user.hashed_password)}"
        )

    print("\n--- Deleting User ---")
    if delete_user("trinity_matrix"):
        print("User deleted successfully.")
