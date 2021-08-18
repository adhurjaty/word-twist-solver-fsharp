with open('wordTwist/words.txt', 'r') as f:
    lines = f.readlines()

formatted = [line.lower() for line in lines if len(line) > 3]

with open('wordTwist/words.txt', 'w') as f:
    f.writelines(formatted)