package ci.gouv.dgbf.system.actor.server.business.api;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public interface RequestBusiness extends BusinessEntity<Request> {

	@Transactional
	void initialize(Request request);
	
	@Transactional
	void record(Request request);
	
	@Transactional
	void submit(Request request);
	
	@Transactional
	void submitByIdentifier(String identifier);
	
	@Transactional
	void accept(Request request);
	
	@Transactional
	void acceptByIdentifier(String identifier);
	
	@Transactional
	void reject(Request request);
	
	@Transactional
	void rejectByIdentifier(String identifier,String rejectionReason);
	
	/**/
	
	/**/
	
	String INITIALIZE = "Request.initialize";
	String RECORD = "Request.record";
	String SUBMIT = "Request.submit";
	String ACCEPT = "Request.accept";
	String REJECT = "Request.reject";
}