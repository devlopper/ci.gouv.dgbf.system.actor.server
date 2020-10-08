package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ExecutionImputationBusinessImpl extends AbstractBusinessEntityImpl<ExecutionImputation, ExecutionImputationPersistence> implements ExecutionImputationBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
