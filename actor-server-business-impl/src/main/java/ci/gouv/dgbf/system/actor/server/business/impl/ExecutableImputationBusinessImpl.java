package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ExecutableImputationBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ExecutableImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutableImputation;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ExecutableImputationBusinessImpl extends AbstractBusinessEntityImpl<ExecutableImputation, ExecutableImputationPersistence> implements ExecutableImputationBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
