package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public interface RequestBusiness extends BusinessEntity<Request> {

	@Transactional
	void initialize(Request request);
	
	@Transactional
	void record(Request request);
	
	@Transactional
	void recordPhoto(Request request);
	
	@Transactional
	void recordPhotoByIdentifier(String identifier,byte[] bytes);
	
	@Transactional
	void recordActOfAppointment(Request request);
	
	@Transactional
	void recordActOfAppointmentByIdentifier(String identifier,byte[] bytes);
	
	@Transactional
	void recordSignature(Request request);
	
	@Transactional
	void recordSignatureByIdentifier(String identifier,byte[] bytes);
	
	@Transactional
	void recordSignedRequestSheet(Request request);
	
	@Transactional
	void recordSignedRequestSheetByIdentifier(String identifier,Boolean isAdministrator,byte[] bytes);
	
	@Transactional
	void submit(Request request);
	
	@Transactional
	void submitByIdentifier(String identifier,String readPageURL);
	
	@Transactional
	void return_(Request request);
	
	@Transactional
	void returnByIdentifier(String identifier,String readPageURL);
	
	@Transactional
	void accept(Request request);
	
	@Transactional
	void acceptByIdentifier(String identifier,Collection<String> grantedBudgetariesScopeFunctionsIdentifiers,String acceptationComment,String readPageURL,String auditActor);
	
	@Transactional
	void reject(Request request);
	
	@Transactional
	void rejectByIdentifier(String identifier,String rejectionReason,String readPageURL,String auditActor);
	
	Integer notifyAccessTokens(String electronicMailAddress,String readPageURL);
	
	@Transactional
	void exportForAccountCreation(String actorCode);
	
	/**/
	
	/**/
	
	String INITIALIZE = "Request.initialize";
	String RECORD = "Request.record";
	String RECORD_PHOTO = "Request.record.photo";
	String RECORD_SIGNATURE = "Request.record.signature";
	String RETURN = "Request.return";
	String SUBMIT = "Request.submit";
	String ACCEPT = "Request.accept";
	String REJECT = "Request.reject";
	String NOTIFY_ACCESS_TOKENS = "Request.notifyAccessTokens";
	String EXPORT_FOR_ACCOUNT_CREATION = "Request.exportForAccountCreation";
	
	/**/
	
	String REPRESENTATION_PATH = "demande";
	String REPRESENTATION_PATH_BUILD_REPORT_BY_IDENTIFIER = "construireetatparidentifiant";
}