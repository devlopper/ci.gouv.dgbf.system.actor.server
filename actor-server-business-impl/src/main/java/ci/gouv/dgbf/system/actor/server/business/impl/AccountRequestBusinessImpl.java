package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.protocol.smtp.MailSender;
import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionRemover;
import ci.gouv.dgbf.system.actor.server.business.api.AccountRequestBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.IdentityBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AccountRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity.Interface;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;

@ApplicationScoped
public class AccountRequestBusinessImpl extends AbstractBusinessEntityImpl<AccountRequest, AccountRequestPersistence> implements AccountRequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public void record(Collection<AccountRequest> accountRequests) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public void submit(Collection<AccountRequest> accountRequests) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public void notifyAccessToken(Collection<AccountRequest> accountRequests) {
		if(CollectionHelper.isEmpty(accountRequests))
			return;
		accountRequests.forEach(accountRequest -> {
			new Thread(new Runnable() {				
				@Override
				public void run() {
					try {
						MailSender.getInstance().send("SIIBC - Demande de compte", "Votre jeton d'accès à votre demande de compte : "+accountRequest.getAccessToken(), accountRequest.getElectronicMailAddress());
					} catch (Exception exception) {
						LogHelper.log(exception, getClass());
					}	
				}
			}).start();			
		});
	}
	
	@Override
	public void notifyAccessToken(AccountRequest... accountRequests) {
		if(ArrayHelper.isEmpty(accountRequests))
			return;
		notifyAccessToken(CollectionHelper.listOf(accountRequests));
	}
	
	@Override @Transactional
	public void accept(Collection<AccountRequest> accountRequests) {
		if(CollectionHelper.isEmpty(accountRequests))
			return;
		LocalDateTime localDateTime = LocalDateTime.now();
		accountRequests.forEach(accountRequest -> {
			//we create actor
			__inject__(ActorBusiness.class).create(new Actor().setCreationDate(localDateTime).setIdentity(accountRequest.getIdentity()));
			//we remote request from pool
			__persistence__.delete(accountRequest);
		});
	}
	
	@Override @Transactional
	public void accept(AccountRequest...accountRequests) {
		if(ArrayHelper.isEmpty(accountRequests))
			return;
		accept(CollectionHelper.listOf(accountRequests));
	}
	
	@Override @Transactional
	public void reject(Collection<AccountRequest> accountRequests) {
		if(CollectionHelper.isEmpty(accountRequests))
			return;
		LocalDateTime localDateTime = LocalDateTime.now();
		accountRequests.forEach(accountRequest -> {
			//we archive rejection
			RejectedAccountRequest rejectedAccountRequest = new RejectedAccountRequest();		
			rejectedAccountRequest.setDate(localDateTime).setElectronicMailAddress(accountRequest.getIdentity().getElectronicMailAddress())
			.setFirstName(accountRequest.getIdentity().getFirstName()).setLastNames(accountRequest.getIdentity().getLastNames()).setReason(null)
			.setRequestDate(accountRequest.getCreationDate());			
			//__inject__(RejectedAccountRequestBusiness.class).create(rejectedAccountRequest);//FIXME WHY NOT WORKING ???
			EntityCreator.getInstance().createOne(rejectedAccountRequest);
			//we remote request from pool
			delete(accountRequest);
		});
	}
	
	@Override @Transactional
	public void reject(AccountRequest...accountRequests) {
		if(ArrayHelper.isEmpty(accountRequests))
			return;
		reject(CollectionHelper.listOf(accountRequests));
	}
	
	@Override
	protected void __listenExecuteCreateBefore__(AccountRequest accountRequest, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(accountRequest, properties, function);
		//we create identity first
		accountRequest.setIdentity(__inject__(IdentityBusiness.class).createFromInterface((Interface) accountRequest));
		if(StringHelper.isBlank(accountRequest.getIdentifier()))
			accountRequest.setIdentifier("DM_"+accountRequest.getElectronicMailAddress());
		accountRequest.setCreationDate(LocalDateTime.now());
		accountRequest.setAccessToken(RandomHelper.getAlphanumeric(3)+"_"+RandomHelper.getAlphanumeric(4)+"_"+RandomHelper.getAlphanumeric(3));
	}
	
	@Override
	protected void __listenExecuteCreateAfter__(AccountRequest accountRequest, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateAfter__(accountRequest, properties, function);
		notifyAccessToken(accountRequest);
	}
	
	@Override
	protected void __listenExecuteDeleteAfter__(AccountRequest accountRequest, Properties properties, BusinessFunctionRemover function) {
		super.__listenExecuteDeleteAfter__(accountRequest, properties, function);
		if(accountRequest.getIdentity() != null)
			__inject__(IdentityBusiness.class).delete(accountRequest.getIdentity());
	}
	
	@Override
	protected Boolean __isCallDeleteByInstanceOnDeleteByIdentifier__() {
		return Boolean.TRUE;
	}
}