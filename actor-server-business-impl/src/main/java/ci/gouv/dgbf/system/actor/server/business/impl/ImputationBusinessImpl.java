package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ImputationBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ImputationBusinessImpl extends AbstractBusinessEntityImpl<Imputation, ImputationPersistence> implements ImputationBusiness,Serializable {
	private static final long serialVersionUID = 1L;

}
