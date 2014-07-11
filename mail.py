import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.Utils import formatdate

def create_html_message(from_addr, to_addr, subject, html):
    body = MIMEText(html, 'html')

    msg = MIMEMultipart('alternative')
    msg['Subject'] = subject
    msg['From'] = from_addr
    msg['To'] = to_addr
    msg['Date'] = formatdate()
    msg.attach(body)

    return msg


def create_subject(r, config):
    subject = ''
    subject += '(' + r['status'] + ') '
    subject += 'New Submission for Problem ' + str(r['problem_id']) + ' '
    subject += '#' + config.getspinach('Evernote', 'tag', 'Enter Evernote Tag:') + ' '
    subject += '@' + config.getspinach('Evernote', 'notebook', 'Enter Evernote Notebook:')

    return subject


def connect_smtp(url, port):
    print 'Connecting with %s:%s...' % (url, port)
    s = smtplib.SMTP(url, port)
    s.ehlo()
    s.starttls()
    s.ehlo()
    return s


def login_smtp(smtp, userid, password):
    print 'Logging in as "' + userid + '"...'
    smtp.login(userid, password)
    return smtp


def send_mail_smtp(smtp, from_addr, to_addr, msg):
    print 'Sending e-mail...'
    smtp.sendmail(from_addr, to_addr, msg.as_string())
    smtp.quit()


def manage_html_message(config, html, r):
    smtp_url = config.getspinach('Gmail', 'url', 'Enter SMTP URL:')
    smtp_port = config.getspinach('Gmail', 'port', 'Enter SMTP Port:')
    s = connect_smtp(smtp_url, smtp_port)

    smtp_id = config.getspinach('Gmail', 'id', 'Enter Gmail address:')
    smtp_pass = config.getpass('Gmail', 'pass', 'Enter Gmail Password:')
    s = login_smtp(s, smtp_id, smtp_pass)

    to_addr = config.getspinach('Evernote', 'address', 'Enter Evernote mailaddress:')
    
    subject = create_subject(r, config)

    msg = create_html_message(smtp_id, to_addr, subject, html)
    send_mail_smtp(s, smtp_id, [to_addr], msg)

