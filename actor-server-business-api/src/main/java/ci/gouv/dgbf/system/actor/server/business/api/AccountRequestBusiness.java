package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;

public interface AccountRequestBusiness extends BusinessEntity<AccountRequest> {

	void notifyAccessToken(Collection<AccountRequest> accountRequests);
	void notifyAccessToken(AccountRequest...accountRequests);
	
	@Transactional
	void record(Collection<AccountRequest> accountRequests);
	
	@Transactional
	void submit(Collection<AccountRequest> accountRequests);
	
	@Transactional
	void accept(Collection<AccountRequest> accountRequests);
	
	@Transactional
	void accept(AccountRequest...accountRequests);
	
	@Transactional
	void reject(Collection<AccountRequest> accountRequests);
	
	@Transactional
	void reject(AccountRequest...accountRequests);
	
	String RECORD = "AccountRequestBusiness.record";
	String SUBMIT = "AccountRequestBusiness.submit";
	String ACCEPT = "AccountRequestBusiness.accept";
	String REJECT = "AccountRequestBusiness.reject";
	
	/**/
	
	static void validateReject(String reason) {
		if(StringHelper.isBlank(reason))
			throw new RuntimeException("Le motif de rejet est obligatoire");
	}
}