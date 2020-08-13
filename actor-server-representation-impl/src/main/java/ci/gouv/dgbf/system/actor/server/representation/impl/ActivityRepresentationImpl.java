package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ActivityRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActivityDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ActivityRepresentationImpl extends AbstractRepresentationEntityImpl<ActivityDto> implements ActivityRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
