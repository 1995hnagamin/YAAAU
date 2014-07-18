import aojtools
import os.path

import SpinachConfigParser
import html_message
import mail

def submit_code(code, problem_id, config):
    user_id = config.getspinach('General', 'aojid',
            'Enter ID for AOJ:')
    
    password = config.getpass('General', 'aojpass',
            'Enter password for %s:' % user_id)

    info = {
            'user_id': user_id,
            'password': password,
            'code': code['body'],
            'problem_id': int(problem_id),
            'lang': code['lang']
            }
    print 'Problem ID: ' + problem_id
    print 'language: ' + code['lang']
    print 'Submitting solution to AOJ...'

    response = aojtools.submit.submit(info)
    return response


def ext2lang(ext):
    table = {
            '.c': 'C',
            '.cc': 'C++',
            '.java': 'JAVA'
            }

    if table.has_key(ext):
        return table[ext]
    else:
        ask = SpinachConfigParser.prompt('Enter Language:')
        return ask()


def read_code(filename):
    f = open(filename)
    code = f.read()

    root, ext = os.path.splitext(filename)
    lang = ext2lang(ext)

    info = {
            'body': code,
            'lang': lang
            }

    return info


def aoj_submit(args):
    config = SpinachConfigParser.SpinachConfigParser('~/.yaaau')

    filename = args.filename
    problem_id = args.problem

    code = read_code(filename)
    if args.language:
        code['lang'] = args.language

    r = submit_code(code, problem_id, config)

    print r['status']
    print 'TIME:' + str(r['cputime']) + ', MEMORY:' + str(r['memory'])

    html = html_message.create_judge_detail(code['body'], r)

    mail.manage_html_message(config, html, r)

