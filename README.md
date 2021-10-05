# Harbour Editor

Is a simple, free source code editor, written in [Harbour](https://harbour.github.io/), open source.

## Install

Clone an existing repository only fetching the 10 most recent commits on the default branch (useful to save time):

```
git clone --depth 10 https://github.com/rjopek/he
```

You can get subsequent updates using this command:

```
git pull
```

#### Linux

Set up a Harbour Editor installation directory for your user account. The following commands will add environment variables to your ~/.bashrc file to configure the Harbour Editor installation path:

```
echo '' >> ~/.bashrc
echo '# Install Harbour Editor to ~/he' >> ~/.bashrc
echo 'export PATH="$HOME/he/bin/linux/gcc:$PATH"' >> ~/.bashrc
source ~/.bashrc
```
