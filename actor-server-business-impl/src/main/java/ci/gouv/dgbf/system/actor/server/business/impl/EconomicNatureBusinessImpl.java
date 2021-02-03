package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.EconomicNatureBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.EconomicNaturePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.EconomicNature;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class EconomicNatureBusinessImpl extends AbstractBusinessEntityImpl<EconomicNature, EconomicNaturePersistence> implements EconomicNatureBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
