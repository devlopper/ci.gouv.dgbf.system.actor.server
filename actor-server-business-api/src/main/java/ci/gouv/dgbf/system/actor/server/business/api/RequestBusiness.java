package ci.gouv.dgbf.system.actor.server.business.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

public interface RequestBusiness extends BusinessEntity<Request> {

	@Transactional
	void initialize(Request request);
	
	@Transactional
	void accept(Request request);
	
	@Transactional
	void acceptByIdentifier(String identifier);
	
	@Transactional
	void reject(Request request);
	
	@Transactional
	void rejectByIdentifier(String identifier,String rejectionReason);
	
	String INITIALIZE = "Request.initialize";
	String ACCEPT = "Request.accept";
	String REJECT = "Request.reject";
}