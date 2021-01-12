prompt = ('> ')
print("""
I'm not in the habit of giving robots my email address.  Are you a robot? Answer 'no' or 'yes'
""")
robotStatus = input(prompt)

if robotStatus == 'no':
  robotStatus1       = str.replace(robotStatus, 'o','e')
  robotStatus2       = str.replace(robotStatus, 'n','h')  
  robotStatusReverse = robotStatus1[::-1]
  print('darr'+robotStatusReverse+robotStatus2+'@'+'ta'+'mu'+'.edu')
else:
  print("either you're a robot or did't follow directions.  Try again only if you're not a robot")
