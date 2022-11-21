# I. Basic idea: 
# On //Apache2 server// side, listen to public port 80 for different URL requests
# When there are requests for URLs bipotentr.cistrome.org, use the corresponding conf files to ProxyPass / redirect requests to port 3838
# Apache2 conf file for bipotentr.cistrome.org is located in /etc/apache2/sites-available/bipotentr.conf
# Include the above conf files in the /etc/apache2/sites-available/000-default.conf
# On //Shiny server// side, listen to port 3838, and define application locations for bipotentr
# reload conf files for both Apache2 server and Shiny server
# ================================================================== #

# II. Operation steps for bipotentr:
# ================================================================== #
# II.0 Copy source code of the app to /project
$ cp /liulab/asahu/projects/gdt/src/bipotentr/v0 /project/

# II.1. Add the apache config
$ cp /liulab/asahu/projects/gdt/src/bipotentr/v0/web_config/bipotentr.conf /etc/apache2/sites-available/bipotentr.conf
# Append 'Include sites-available/bipotentr.conf' to /etc/apache2/sites-available/000-default.conf 
$ echo 'Include sites-available/bipotentr.conf' >> /etc/apache2/sites-available/000-default.conf

# II.2. Add config of shiny server
# Append the contents of shiny-server.conf to /etc/shiny-server/shiny-server.conf using vi
$ cat /liulab/asahu/projects/gdt/src/bipotentr/v0/web_config/shiny-server.conf >> /etc/shiny-server/shiny-server.conf


# II.3. Reload Apache2 and Shiny server configurations
$ /etc/init.d/apache2 reload
$ sudo /bin/systemctl restart shiny-server
