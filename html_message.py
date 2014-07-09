def html_table_row(title, data):
    return '<tr><td>' + title + '</td><td>' + data + '</td></tr>'


def html_a(uri, content):
    return '<a href="' + uri + '">' + content + '</a>'


def html_color(color, content):
    return '<span style="font-weight:bold;color:' + color + ';">' + content + '</span>'


def html_judge_status(s):
    if s == 'Accepted':
        return html_color('Green', s)
    elif s == 'Wrong Answer':
        return html_color('Red', s)
    elif s == 'Time Limit Exceeded' or s == 'Memory Limit Exceeded' or s == 'Output Limit Exceeded':
        return html_color('Yellow', s)
    elif s == 'Runtime Error' or s == 'Presentation Error':
        return html_color('PaleVioletRed', s)
    else:
        return html_color('DarkGray', s)


def code2html(code):
    def f(n):
        h = htmlentitydefs.codepoint2name
        if n in h:
            return "&%s;" % h[n]
        elif n < 256:
            return unichr(n)
        else:
            return "&%d;" % n
    return "".join(f(ord(c)) for c in code)

def create_subject(r, config):
    subject = ''
    subject += '(' + r['status'] + ') '
    subject += 'New Submission for Problem ' + str(r['problem_id']) + ' '
    subject += '#' + config.getspinach('Evernote', 'tag', 'Enter Evernote Tag:') + ' '
    subject += '@' + config.getspinach('Evernote', 'notebook', 'Enter Evernote Notebook:')

    return subject

def create_judge_detail(code, response):
    body = '<html><head></head><body>'
    body += '<p><table border="0">'

    rid = response['run_id']
    rlink = html_a('http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=' + str(rid), str(rid))
    body += html_table_row('RID', rlink)

    pid = str(response['problem_id'])
    ptable = api.problem(id=pid)
    plink = html_a('http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=' + pid, pid + ': ' + ptable['name'])
    body += html_table_row('Problem', plink)

    date = response['submission_date_str']
    body += html_table_row('Submission Date', date)
    
    judge = response['status']
    body += html_table_row('Status', html_judge_status(judge))

    time = response['cputime']
    body += html_table_row('Time', str(time))

    memory = response['memory']
    body += html_table_row('Memory', str(memory))

    lang = response['language']
    body += html_table_row('Language', lang)

    size = response['code_size']
    body += html_table_row('Code Size', str(size))

    body += '</table></p>'

    body += '<p><pre><code>'
    body += code2html(code)

    body += '</code></pre></p></body></html>'

    return body

