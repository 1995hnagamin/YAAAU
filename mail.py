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
    subject += '#' + config.getspinach('submit',
            'tag', 'Enter Evernote Tag:') + ' '

    subject += '@' + config.getspinach('submit', 'notebook', 'Enter Evernote Notebook:')

    return subject


def connect_smtp(url, port):
    print 'Connecting with %s:%s...' % (url, port)
    s = smtplib.SMTP(url, port)
    s.ehlo()
    s.starttls()
    s.ehlo()
    return s


def login_smtp(smtp, userid, password):
    print 'Logging in as ' + userid + '...'
    smtp.login(userid, password)
    return smtp


def send_mail_smtp(smtp, from_addr, to_addr, msg):
    print 'Sending email...'
    smtp.sendmail(from_addr, to_addr, msg.as_string())
    smtp.quit()


def manage_html_message(config, html, r):
    smtp_url = config.getspinach('submit', 'url', 'Enter SMTP URL:')
    smtp_port = config.getspinach('submit', 'port', 'Enter Port for %s:' % smtp_url)
    s = connect_smtp(smtp_url, smtp_port)

    smtp_id = config.getspinach('submit', 'id',
            'Enter ID for %s' % smtp_url)
    smtp_pass = config.getpass('submit', 'pass', 'Enter password for %s:' % smtp_id)
    s = login_smtp(s, smtp_id, smtp_pass)

    to_addr = config.getspinach('submit', 'destination', 'Enter destination mailaddress:')
    
    subject = create_subject(r, config)

    msg = create_html_message(smtp_id, to_addr, subject, html)
    send_mail_smtp(s, smtp_id, [to_addr], msg)

