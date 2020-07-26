package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ActorFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ActorFunctionBusinessImpl extends AbstractBusinessEntityImpl<ActorFunction, ActorFunctionPersistence> implements ActorFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
