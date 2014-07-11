import aojtools
import os.path

import SpinachConfigParser
import html_message
import mail

def submit_code(code, problem_id, config):
    user_id = config.getspinach('AOJ', 'id', 'Enter AOJ ID:')
    password = config.getpass('AOJ', 'pass', 'Enter AOJ Password:')
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
    if ext == '.cc':
        lang = 'C++'
    elif ext == '.java':
        lang = 'JAVA'
    elif ext == '.c':
        lang = 'C'
    else:
        lang = ''

    return lang


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
    config = SpinachConfigParser.SpinachConfigParser('~/.yaau')

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

