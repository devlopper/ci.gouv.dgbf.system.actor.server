package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.AccountRequestBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.IdentityBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AccountRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity.Interface;

import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;

@ApplicationScoped
public class AccountRequestBusinessImpl extends AbstractBusinessEntityImpl<AccountRequest, AccountRequestPersistence> implements AccountRequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenExecuteCreateBefore__(AccountRequest accountRequest, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(accountRequest, properties, function);
		//we create identity first
		accountRequest.setIdentity(__inject__(IdentityBusiness.class).createFromInterface((Interface) accountRequest));
		if(StringHelper.isBlank(accountRequest.getIdentifier()))
			accountRequest.setIdentifier("DM_"+accountRequest.getElectronicMailAddress());
	}
	
}
