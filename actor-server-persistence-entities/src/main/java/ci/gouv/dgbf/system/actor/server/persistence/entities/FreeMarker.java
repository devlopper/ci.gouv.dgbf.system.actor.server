package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.util.Map;

import org.cyk.utility.__kernel__.string.StringGenerator;
import org.cyk.utility.__kernel__.string.freemarker.FreeMarkerHelper;

import freemarker.cache.ClassTemplateLoader;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateExceptionHandler;

public class FreeMarker {

	public static String getRecordedMailMessage(AccountRequest accountRequest) {
		return StringGenerator.getInstance().generate(FreeMarker.getMailTemplate("account_request_saved.ftlh")
				, Map.of("names",Identity.getNames(accountRequest)));
	}
	
	public static String getSubmittedMailMessage(AccountRequest accountRequest) {
		return StringGenerator.getInstance().generate(FreeMarker.getMailTemplate("account_request_submitted.ftlh")
				, Map.of("names",Identity.getNames(accountRequest)));
	}
	
	public static String getAcceptedMailMessage(Actor actor) {
		return StringGenerator.getInstance().generate(FreeMarker.getMailTemplate("account_request_accepted.ftlh")
				, Map.of("names",Identity.getNames(actor),"username",actor.getCode(),"password",actor.getPassword()));
	}
	
	public static String getRejectedMailMessage(RejectedAccountRequest rejectedAccountRequest) {
		return StringGenerator.getInstance().generate(FreeMarker.getMailTemplate("account_request_rejected.ftlh")
				, Map.of("names",Identity.getNames(rejectedAccountRequest),"reason",rejectedAccountRequest.getReason()));
	}
	
	public static String getCreatedMailMessage(Actor actor) {
		return StringGenerator.getInstance().generate(FreeMarker.getMailTemplate("actor_created.ftlh")
				, Map.of("names",Identity.getNames(actor),"username",actor.getCode(),"password",actor.getPassword()));
	}
	
	/**/
	
	public static final Configuration CONFIGURATION = new Configuration(Configuration.VERSION_2_3_29);
	
	public static Template getMailTemplate(String identifier) {
		return FreeMarkerHelper.getTemplate(CONFIGURATION, "mail/"+identifier);
	}
	
	static {
        CONFIGURATION.setDefaultEncoding("UTF-8");
        CONFIGURATION.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
        CONFIGURATION.setLogTemplateExceptions(false);
        CONFIGURATION.setWrapUncheckedExceptions(true);
        CONFIGURATION.setFallbackOnNullLoopVariable(false);
        CONFIGURATION.setTemplateLoader(new ClassTemplateLoader(FreeMarkerHelper.class, "/ci/gouv/dgbf/system/actor/server/persistence/entities"));
	}
	
}