package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;

public interface AccountRequestBusiness extends BusinessEntity<AccountRequest> {

	@Transactional
	void accept(Collection<AccountRequest> accountRequests);
	
	@Transactional
	void accept(AccountRequest...accountRequests);
	
	@Transactional
	void reject(Collection<AccountRequest> accountRequests);
	
	@Transactional
	void reject(AccountRequest...accountRequests);
	
	String ACCEPT = "AccountRequestBusiness.accept";
	String REJECT = "AccountRequestBusiness.reject";
}