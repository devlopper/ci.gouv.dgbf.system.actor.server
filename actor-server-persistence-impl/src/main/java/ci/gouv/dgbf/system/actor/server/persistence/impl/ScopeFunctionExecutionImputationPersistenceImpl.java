package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ScopeFunctionExecutionImputationPersistenceImpl extends AbstractPersistenceEntityImpl<ScopeFunctionExecutionImputation> implements ScopeFunctionExecutionImputationPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}