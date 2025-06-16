# ABAP AI Tools
This repository provides a set of tools designed to facilitate Artificial Intelligence capabilities within ABAP environments.

## Installation
You can install the ABAP AI Tools into your SAP system using abapGit. Follow the steps below:

## Prerequisites
 - abapGit: Ensure that `abapGit` is installed and configured in your ABAP system. If not, you can find the latest version and installation instructions on the official abapGit website: https://docs.abapgit.org/
 - Developer Access: You need appropriate developer authorizations in your ABAP system to import objects.

## Installation Steps
1 - Open `abapGit`: In your SAP GUI, execute transaction `ZABAPGIT` (or the equivalent transaction code you have set up for abapGit).

2 - Add Online Repository:
  - Click on the `+` button (Add Online Repo) or select "New Online" from the menu.

3 - Enter Repository URL:
  - In the "URL" field, paste the URL of this GitHub repository: `https://github.com/christianjianelli/yaai`
  - For the Package, we recommend creating a new package called `YAAI`. Remember to assign it to a transport request if necessary.
  - Click "OK" or press Enter.

4 - Clone Repository:
  - `abapGit` will display the repository details. Review the objects that will be imported.
  - Click the "Clone" button (often represented by a green download icon).

5 - Activate Objects:
  - Once the cloning process is complete, `abapGit` will list the imported objects.
  - Activate any inactive objects if prompted.

6 - Verify Installation:
  - After activation, all the `ABAP AI Tools` objects (interfaces, classes, etc.) will be available in your specified package. You can verify this by checking transaction `SE80` for the package you used.

You have now successfully installed the `ABAP AI Tools!`
