package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ActivityEconomicNatureRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActivityEconomicNatureDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ActivityEconomicNatureRepresentationImpl extends AbstractRepresentationEntityImpl<ActivityEconomicNatureDto> implements ActivityEconomicNatureRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
