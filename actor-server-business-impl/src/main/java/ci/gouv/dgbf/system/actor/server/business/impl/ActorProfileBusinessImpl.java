package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.business.server.EntityCreator;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.security.keycloak.server.UserManager;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionRemover;

import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;

@ApplicationScoped
public class ActorProfileBusinessImpl extends AbstractBusinessEntityImpl<ActorProfile, ActorProfilePersistence> implements ActorProfileBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	public static TransactionResult create(ActorProfile actorProfile,EntityManager entityManager) {
		if(StringHelper.isBlank(actorProfile.getIdentifier()) && actorProfile.getActor() != null && actorProfile.getProfile() != null)
			actorProfile.setIdentifier(actorProfile.getActor().getCode()+"_"+actorProfile.getProfile().getCode());
		EntityCreator.getInstance().create(new QueryExecutorArguments().setObjects(List.of(actorProfile)).setEntityManager(entityManager));
		try {
			UserManager.getInstance().addRolesByNames(actorProfile.getActor().getCode(), actorProfile.getProfile().getCode());			
		} catch (Exception exception) {
			LogHelper.logWarning(String.format("role %s not added in keycloak on user %s",actorProfile.getProfile().getCode(),actorProfile.getActor().getCode()), ActorProfileBusinessImpl.class);
		}
		return null;
	}
	
	@Override
	protected void __listenExecuteCreateBefore__(ActorProfile actorProfile, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(actorProfile, properties, function);
		if(StringHelper.isBlank(actorProfile.getIdentifier()) && actorProfile.getActor() != null && actorProfile.getProfile() != null)
			actorProfile.setIdentifier(actorProfile.getActor().getCode()+"_"+actorProfile.getProfile().getCode());
	}
	
	@Override
	protected void __listenExecuteCreateAfter__(ActorProfile actorProfile, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateAfter__(actorProfile, properties, function);
		try {
			UserManager.getInstance().addRolesByNames(actorProfile.getActor().getCode(), actorProfile.getProfile().getCode());			
		} catch (Exception exception) {
			LogHelper.logWarning(String.format("role %s not added in keycloak on user %s",actorProfile.getProfile().getCode(),actorProfile.getActor().getCode()), getClass());
		}
	}
	
	@Override
	protected void __listenExecuteDeleteAfter__(ActorProfile actorProfile, Properties properties,BusinessFunctionRemover function) {
		super.__listenExecuteDeleteAfter__(actorProfile, properties, function);
		try {
			UserManager.getInstance().deleteRolesByNames(actorProfile.getActor().getCode(), actorProfile.getProfile().getCode());			
		} catch (Exception exception) {
			LogHelper.logWarning(String.format("role %s not deleted in keycloak on user %s",actorProfile.getProfile().getCode(),actorProfile.getActor().getCode()), getClass());
		}
	}
}