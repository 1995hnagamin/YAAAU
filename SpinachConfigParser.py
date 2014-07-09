def prompt(message):
    def func():
        print message,
        return raw_input()

    return func

def pass_prompt(message):
    def func():
        return getpass.getpass(prompt=message)
    
    return func


class SpinachConfigParser:
    def __init__(self, filepath):
        self.config = ConfigParser.SafeConfigParser()
        self.config.read(expanduser(filepath))
    
    def get(self, section, option, default):
        if self.config.has_option(section, option):
            return self.config.get(section, option)
        else:
            return default()

    def getspinach(self, section, option, message):
        return self.get(section, option, prompt(message))

    def getpass(self, section, option, message):
        return self.get(section, option, pass_prompt(message))

