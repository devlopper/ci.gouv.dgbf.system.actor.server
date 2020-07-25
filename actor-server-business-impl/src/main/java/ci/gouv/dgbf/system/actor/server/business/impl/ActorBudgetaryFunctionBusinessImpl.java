package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ActorBudgetaryFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorBudgetaryFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorBudgetaryFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ActorBudgetaryFunctionBusinessImpl extends AbstractBusinessEntityImpl<ActorBudgetaryFunction, ActorBudgetaryFunctionPersistence> implements ActorBudgetaryFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
