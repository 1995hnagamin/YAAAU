import ConfigParser
import os.path
import getpass


class SpinachConfigParser:
    def __init__(self, filepath):
        self.config = ConfigParser.SafeConfigParser()
        path = os.path.expanduser(filepath)
        self.config.read(path)
    
    def get(self, section, option, default):
        if self.config.has_option(section, option):
            return self.config.get(section, option)
        else:
            return default()

    def getspinach(self, section, option, message):
        return self.get(section, option, prompt(message))

    def getpass(self, section, option, message):
        return self.get(section, option, pass_prompt(message))


class SpinachDecorator:
    def __init__(self, config, message, validate):
        self.config = config
        self.message = message
        self.validate = validate

    def get(self, section, option):
        if self.config.has_option(section, option):
            return self.config.get(section, option)
        else:
            print self.message,
            value = raw_input()
            if valudate(value):
                return value
            else:
                return self.get(section, option)

    def has_option(self, section, opstion):
        return self.has_option(section, option)


class DefaultDecorator:
    def __init__(self, config, dictionary):
        self.config = config
        self.defaults = dictionary

    def get(self, section, option):
        if self.config.has_option(section, option):
            return self.config.get(section, option)
        elif (section, option) in self.defaults:
            return self.defaults[ (section, option) ]
        else:
            raise ConfigParser.NoOptionError

    def has_option(self, section, option):
        return self.defaults.has_key( (section, option) ) or self.config.has_key(section, option)


class PasswordDecorator:
    def __init__(self, config, dictionary, message, validate):
        self.config = config
        self.need_to_hide = dictionary
        self.message = message
        self.validate = validate

    def get(self, section, option):
        if self.config.has_option(section, option):
            return self.config.get(section, option)
        elif (section, option) in self.need_to_hide:
            return getpass.getpass(prompt=self.message)
        else:
            raise ConfigParser.NoOptionError

    def has_option(self, section, option):
        return 
