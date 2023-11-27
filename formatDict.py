with open('words.txt', 'r') as f:
    lines = f.readlines()

formatted = [line.lower() for line in lines if len(line) > 3]

with open('words.txt', 'w') as f:
    f.writelines(formatted)