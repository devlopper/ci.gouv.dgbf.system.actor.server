package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ActivityEconomicNatureBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActivityEconomicNaturePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityEconomicNature;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ActivityEconomicNatureBusinessImpl extends AbstractBusinessEntityImpl<ActivityEconomicNature, ActivityEconomicNaturePersistence> implements ActivityEconomicNatureBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
