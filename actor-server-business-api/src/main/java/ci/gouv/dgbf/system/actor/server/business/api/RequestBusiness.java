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
	void submitByIdentifier(String identifier);
	
	@Transactional
	void accept(Request request);
	
	@Transactional
	void acceptByIdentifier(String identifier,Collection<String> budgetariesScopeFunctionsIdentifiers,String comment,byte[] signedRequestSheetBytes);
	
	@Transactional
	void reject(Request request);
	
	@Transactional
	void rejectByIdentifier(String identifier,String rejectionReason);
	
	Integer notifyAccessTokens(String electronicMailAddress,String readPageURL);
	
	/**/
	
	/**/
	
	String INITIALIZE = "Request.initialize";
	String RECORD = "Request.record";
	String RECORD_PHOTO = "Request.record.photo";
	String RECORD_SIGNATURE = "Request.record.signature";
	String SUBMIT = "Request.submit";
	String ACCEPT = "Request.accept";
	String REJECT = "Request.reject";
	String NOTIFY_ACCESS_TOKENS = "Request.notifyAccessTokens";
	
	/**/
	
	String REPRESENTATION_PATH = "demande";
	String REPRESENTATION_PATH_BUILD_REPORT_BY_IDENTIFIER = "construireetatparidentifiant";
}