package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ExecutionImputationPersistenceImpl extends AbstractPersistenceEntityImpl<ExecutionImputation> implements ExecutionImputationPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}