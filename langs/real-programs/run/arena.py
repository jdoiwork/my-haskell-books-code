# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
import random

hero_hp, monster_hp = 50, 40
while True:
    print(f"Hero HP: {hero_hp}  ⚔️  Slime HP: {monster_hp}")
    command = input("Command (attack / heal / run)> ")
    if command in ("run", "r"):
        print("You ran away safely.")
        break
    if command in ("attack", "a"):
        monster_hp = max(0, monster_hp - random.randint(8, 14))
    elif command in ("heal", "h"):
        hero_hp = min(50, hero_hp + random.randint(5, 10))
    else:
        print("Huh? Try again.")
        continue
    if monster_hp == 0:
        print("The Slime is defeated. You win!")
        break
    hero_hp = max(0, hero_hp - random.randint(3, 9))
    if hero_hp == 0:
        print("You were defeated... Game over.")
        break
# ANCHOR_END: procedural
