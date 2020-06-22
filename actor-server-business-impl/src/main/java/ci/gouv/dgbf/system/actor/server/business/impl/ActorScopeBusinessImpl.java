package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;

import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;

@ApplicationScoped
public class ActorScopeBusinessImpl extends AbstractBusinessEntityImpl<ActorScope, ActorScopePersistence> implements ActorScopeBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected void __listenExecuteCreateBefore__(ActorScope actorScope, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(actorScope, properties, function);
		if(StringHelper.isBlank(actorScope.getIdentifier()) && actorScope.getActor() != null && actorScope.getScope() != null && actorScope.getScope().getType() != null)
			actorScope.setIdentifier(actorScope.getActor().getCode()+"_"+actorScope.getScope().getType().getCode()+"_"+actorScope.getScope().getCode());
	}
	
}
