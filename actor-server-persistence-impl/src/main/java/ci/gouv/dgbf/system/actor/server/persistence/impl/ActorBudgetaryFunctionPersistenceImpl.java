package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActorBudgetaryFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorBudgetaryFunction;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActorBudgetaryFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<ActorBudgetaryFunction> implements ActorBudgetaryFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}