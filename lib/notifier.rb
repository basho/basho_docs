require 'gmail'

def notify(success, error=nil)
  email = ENV['DEPLOY_EMAIL']
  email_pw = ENV['DEPLOY_EMAIL_PW']
  email_list = ENV['DEPLOY_EMAIL_LIST'] || 'docs@basho.com'
  if error
    reason = error.message + "\n"
    reason += error.backtrace.join("\n")
  end

  if email && email_pw
    Gmail.connect(email, email_pw) do |gmail|
      break unless gmail.logged_in?
      if success
        subjecttxt = "[Docs] Basho Docs have been deployed"
        bodytxt = "The Basho Docs have been deployed, but give it a\n"
        bodytxt += "few minutes while the CDN cache is invalidated."
      else
        subjecttxt = "[Docs] Basho Docs deployment failed"
        bodytxt = reason
      end
      gmail.deliver do
        to email_list
        subject subjecttxt
        text_part do
          body bodytxt
        end
      end
      puts "== Status Notified"
    end
  else
    puts "== No Notification Vars Set"
  end
end
