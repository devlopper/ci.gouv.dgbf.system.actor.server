package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;

import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;

@ApplicationScoped
public class ActorProfileBusinessImpl extends AbstractBusinessEntityImpl<ActorProfile, ActorProfilePersistence> implements ActorProfileBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected void __listenExecuteCreateBefore__(ActorProfile actorProfile, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(actorProfile, properties, function);
		if(StringHelper.isBlank(actorProfile.getIdentifier()) && actorProfile.getActor() != null && actorProfile.getProfile() != null)
			actorProfile.setIdentifier(actorProfile.getActor().getCode()+"_"+actorProfile.getProfile().getCode());
	}
	
}
