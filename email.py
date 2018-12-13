prompt = ('> ')
print """
I'm not in the habit of giving robots my email address.  Are you a robot? Answer 'no' or 'yes'
"""
robotStatus = raw_input(prompt)

if robotStatus == 'no':
  robotStatus = str.replace(robotStatus, 'n','m')
  robotStatusReverse = robotStatus[::-1]
  print 'dh'+robotStatusReverse+'righ'+'@'+'gmail.c'+robotStatusReverse
else:
  print "either you're a robot or did't follow directions.  Try again only if you're not a robot"
