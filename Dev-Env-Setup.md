# Development Environment Setup

The following instructions will help you set up your development for our mob session. It is important that the dev environment is set up PRIOR to the mob session.  
The mob session will likely take several hours and not having the dev environment set up prior will likely add a few more hours.

1. Clone this repo to the place of your choice
  - From the main repo screen, copy the https code for this repo.
  - From command line: cd <destination you wish to store the local project folder>
  - Once you verify you are in the file path that you want: git clone <past https code copied from repo>
  - You will now recieve text from the command line letting you know the objects were recieved.  
2. Download Visual Studio Code (I chose to use Visual Studio Code for Junit integration but other IDEs will work as well). If you choose a different set up, please **verify** it works before the mob!
  - If you choose to download Visual Studio Code, please use the following link: https://code.visualstudio.com/download
  - If you already have VS Code installed, from command line: "code -v" will let you know what version you have.
  - We must now download the appropriate Java Extensions for VS Code
    - On the left hand task bar, click the extensions button (Looks like a square with for quadrants and one of the quadrants is being removed).
    - Search "Extension Pack for Java" and you should see the Extension Pack for Java by Microsoft appear. Click the "Install" button.
3. Download Java SE and Verify it is Installed
  - In the command line: java -version (this will let you know if java is installed and what version you have.
  - If you do not have Java installed, go to: https://www.oracle.com/java/technologies/downloads/ and follow the download/setup instructions.
  - To check if you need to update Java SE: type "Configure Java" in the search bar of Windows 10.
    - Run the application
    - Click the "Update Tab"
    - Click "Update Now" at the bottom of the screen.
    - Java SE should now be updated.
4. Download JUnit
    - Click: https://github.com/junit-team/junit4/wiki/Download-and-Install
    -This will download the .jar required. Click the download and follow set up instructions.
