# The Python code in the "typed SQL" chapter (SQLAlchemy 2).
# 「型で SQL を守る」の章の Python のコード（SQLAlchemy 2）

# ANCHOR: sqlalchemy
# Python (SQLAlchemy)
from sqlalchemy import ForeignKey, create_engine, select
from sqlalchemy.orm import DeclarativeBase, Mapped, Session, mapped_column

class Base(DeclarativeBase):
    pass

class Adventurer(Base):
    __tablename__ = "adventurers"
    id: Mapped[int] = mapped_column(primary_key=True)
    name: Mapped[str] = mapped_column(unique=True)
    job: Mapped[str]
    level: Mapped[int]

class Quest(Base):
    __tablename__ = "quests"
    id: Mapped[int] = mapped_column(primary_key=True)
    title: Mapped[str]
    taken_by: Mapped[int | None] = mapped_column(ForeignKey("adventurers.id"))

def strong(session, lv):
    query = (select(Adventurer)
             .where(Adventurer.level >= lv)
             .order_by(Adventurer.level.desc()))
    return session.scalars(query).all()

def board(session):
    query = (select(Quest.title, Adventurer.name)
             .outerjoin(Adventurer, Adventurer.id == Quest.taken_by))
    return session.execute(query).all()
# ANCHOR_END: sqlalchemy

engine = create_engine("sqlite:///:memory:")
Base.metadata.create_all(engine)
with Session(engine) as session:
    lam = Adventurer(name="Lambda", job="Mage", level=3)
    session.add_all([lam, Adventurer(name="Python", job="Archer", level=5),
                     Adventurer(name="Rust", job="Knight", level=1)])
    session.flush()
    session.add_all([Quest(title="Slay the slime", taken_by=lam.id),
                     Quest(title="Find the herb", taken_by=None)])
    session.commit()
    assert [a.name for a in strong(session, 3)] == ["Python", "Lambda"]
    assert [tuple(r) for r in board(session)] == [
        ("Slay the slime", "Lambda"), ("Find the herb", None)]
    # A typo in a column name is found only when that line runs.
    # 列名の打ちまちがいは、その行を実行したときに初めてわかる
    try:
        select(Adventurer).where(Adventurer.levle >= 3)
        raise AssertionError("should fail")
    except AttributeError as e:
        assert "levle" in str(e)
print("ok")
