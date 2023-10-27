import sys, paramiko

hostname = 'wuxpihosl01b.seagate.com'
username = '534180' 
password = 'StarWAys01.*200430z'

ssh_client = paramiko.SSHClient()
ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
ssh_client.connect(hostname=hostname,username=username,password=password)

com_line = "sh /srv/shiny-server/CCP_Auto_Trigger/sh_file/ccp_auto_trigger.sh >> /srv/shiny-server/CCP_Auto_Trigger/sh_file/ccp_auto_trigger.log 2>&1"

stdin, stdout, stderr = ssh_client.exec_command(com_line) 

exit_status = stdout.channel.recv_exit_status()
 
if exit_status == 0:
    print ("Script Completed")
else:
    print("Error", exit_status)

ssh_client.close() 