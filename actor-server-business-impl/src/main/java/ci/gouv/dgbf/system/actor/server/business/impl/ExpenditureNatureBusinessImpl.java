package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ExpenditureNatureBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ExpenditureNaturePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ExpenditureNatureBusinessImpl extends AbstractBusinessEntityImpl<ExpenditureNature, ExpenditureNaturePersistence> implements ExpenditureNatureBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
