prompt = ('> ')
print """
I'm not in the habit of giving robots my email address.  Are you a robot? Answer 'no' or 'yes'
"""
robotStatus = raw_input(prompt)

if robotStatus == 'no':
  robotStatus = str.replace(robotStatus, 'o','@')
  print 'dhomrighause'+robotStatus+'smu.edu'
else:
  print "either you're a robot or did't follow directions.  Try again only if you're not a robot"
