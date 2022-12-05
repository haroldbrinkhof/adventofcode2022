import re

day1_file = open("day1_input", 'r')
values = day1_file.readlines()
current = 0
totals = []
for line in values:
    value = line.strip()
    if re.match("^\d+$", value):
            current += int(value)
    else:
        totals.append(current)
        current = 0

totals.sort()
totals.reverse()

current = 0
for total in totals[:3]:
    current += total

print(current)




